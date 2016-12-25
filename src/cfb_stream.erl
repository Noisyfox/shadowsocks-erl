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

do_block_cipher(#cfb_context{type=encrypt, cipher = Cipher, iv = IV, key = Key}, Input)->
  crypto:block_encrypt(Cipher, Key, IV, Input);
do_block_cipher(#cfb_context{type=decrypt, cipher = Cipher, iv = IV, key = Key}, Input)->
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

extract_iv(Output, OutputSize, BlockSize, RemainSize)->
  HeadSize = OutputSize - BlockSize - RemainSize,
  <<_:(HeadSize)/bitstring, IV:(BlockSize)/bitstring, _:(RemainSize)/bitstring>> = Output,
  IV.
