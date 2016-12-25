%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 十二月 2016 17:47
%%%-------------------------------------------------------------------
-module(cfb_stream).
-author("Noisyfox").

-include_lib("eunit/include/eunit.hrl").

-record(cfb_context, {
  type,
  cipher,
  block_size,
  iv,
  key,
  block_offset = 0,
  last_block = []
}).

%% API
-export([init_context/5, cipher_update/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

init_context(Cipher, Type, BlockSize, Key, IV)
  when ((Type =:= encrypt) or (Type =:= decrypt))
  and is_integer(BlockSize) and (BlockSize > 0) and is_binary(IV) and (bit_size(IV) =:= BlockSize) and is_bitstring(Key) ->
  #cfb_context{cipher = Cipher, type = Type, block_size = BlockSize, iv = IV, key = Key}.

cipher_update(Context, Input) ->
  {Padding, PaddingInput} = padding_input(Context, Input),
  Output = do_block_cipher(Context, PaddingInput),
  NewContext = update_context(Context, Input, Output),
  NewOutput = unpadding_output(Padding, Output),
  {NewContext, NewOutput}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_block_cipher(#cfb_context{type = encrypt, cipher = Cipher, iv = IV, key = Key}, Input) ->
  crypto:block_encrypt(Cipher, Key, IV, Input);
do_block_cipher(#cfb_context{type = decrypt, cipher = Cipher, iv = IV, key = Key}, Input) ->
  crypto:block_decrypt(Cipher, Key, IV, Input).

padding_input(#cfb_context{block_offset = 0}, Input) ->
  {0, Input};
padding_input(#cfb_context{block_offset = Padding, last_block = LastBlock}, Input) ->
  {Padding, [LastBlock, Input]}.

unpadding_output(0, Output) ->
  Output;
unpadding_output(Padding, Output) ->
  <<_:Padding/bitstring, NewOutput/bitstring>> = Output,
  NewOutput.

update_context(#cfb_context{block_size = BlockSize} = Context, Input, Output) ->
  BlockedSize = bit_size(Output),
  if
    BlockedSize < BlockSize -> % no need to update iv, just update last_block
      NewLastBlock = [Context#cfb_context.last_block, Input],
      NewBlockOffset = BlockedSize,
      Context#cfb_context{block_offset = NewBlockOffset, last_block = NewLastBlock};
    true -> % need to get new iv from output
      RemainSize = BlockedSize rem BlockSize,
      case RemainSize of
        0 ->
          NewIV = extract_iv(Output, BlockedSize, BlockSize, 0),
          Context#cfb_context{iv = NewIV, block_offset = 0, last_block = []};
        _ ->
          FlatInput = iolist_to_binary(Input),
          #cfb_context{block_offset = OldOffset} = Context,
          HeadSize = BlockedSize - OldOffset - RemainSize,
          <<_:(HeadSize)/bitstring, NewLastBlock/bitstring>> = FlatInput,
          NewIV = extract_iv(Output, BlockedSize, BlockSize, RemainSize),
          Context#cfb_context{iv = NewIV, block_offset = RemainSize, last_block = NewLastBlock}
      end
  end.

extract_iv(Output, OutputSize, BlockSize, RemainSize) ->
  HeadSize = OutputSize - BlockSize - RemainSize,
  <<_:(HeadSize)/bitstring, IV:(BlockSize)/bitstring, _:(RemainSize)/bitstring>> = Output,
  IV.


%%%===================================================================
%%% Test cases
%%%===================================================================

cipher_test_() ->
  {timeout, 40, fun() -> test_cipher() end}.

test_cipher() ->
  TestCases = [
    {aes_cfb128, 128, 128},
    {aes_cfb128, 128, 192},
    {aes_cfb128, 128, 256},
    {blowfish_cfb64, 64, 128},
    {blowfish_cfb64, 64, 192},
    {blowfish_cfb64, 64, 256},
    {blowfish_cfb64, 64, 448}
  ],
  [[test_cipher(Cipher, BlockSize, KeySize) || _ <- lists:seq(1, 500)]
    || {Cipher, BlockSize, KeySize} <- TestCases],
  ok.

test_cipher(Cipher, BlockSize, KeySize) ->
  test_cipher(encrypt, Cipher, BlockSize, KeySize),
  test_cipher(decrypt, Cipher, BlockSize, KeySize),
  ok.

test_cipher(Type, Cipher, BlockSize, KeySize) ->
  Key = crypto:strong_rand_bytes(KeySize div 8),
  IV = crypto:strong_rand_bytes(BlockSize div 8),
  Context = init_context(Cipher, Type, BlockSize, Key, IV),

  BlockCount = rand:uniform(1000) + 1000, % 1001 ~ 2000 blocks
  InputSize = BlockCount * BlockSize,

  TestData = crypto:strong_rand_bytes(InputSize div 8),

  BlockOutput = case Type of
                  encrypt -> crypto:block_encrypt(Cipher, Key, IV, TestData);
                  decrypt -> crypto:block_decrypt(Cipher, Key, IV, TestData)
                end,

  StreamOutput = test_stream_cipher(Context, TestData, InputSize div 8, <<>>),

  BlockOutput =:= StreamOutput.

test_stream_cipher(_, _, 0, Output) ->
  Output;
test_stream_cipher(Context, RemainData, RemainLen, Output) ->
  ThisLen = rand:uniform(RemainLen),
  <<ThisData:ThisLen/bytes, NextRemain/bytes>> = RemainData,
  {NewContext, ThisOutput} = cipher_update(Context, ThisData),
  NewOutput = <<Output/binary, ThisOutput/binary>>,

  test_stream_cipher(NewContext, NextRemain, RemainLen - ThisLen, NewOutput).
