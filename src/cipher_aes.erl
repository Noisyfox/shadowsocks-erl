%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十二月 2016 0:38
%%%-------------------------------------------------------------------
-module(cipher_aes).
-author("Noisyfox").

-behavior(gen_cipher).

-record(cipher_info, {
  name,
  cipher_type, % cipher_type = stream | cfb
  cipher,
  iv_size,
  key_size
}).

-record(cipher_state, {
  context,
  cipher_info,
  type % type = encrypt, decrypt
}).

-record(state, {
  key,
  cipher_info,
  encrypt_state = undefined,
  decrypt_state = undefined
}).

%% API
-export([init/1, close/2, encrypt/3, decrypt/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init(_Side) ->
  CipherInfo = ciphers("bf-cfb"),
  Key = binary_utils:password_to_key(<<"123456"/utf8>>, CipherInfo#cipher_info.key_size div 8),

  State = #state{
    key = Key,
    cipher_info = CipherInfo
  },
  {ok, State}.

close(_Side, _State) ->
  {ok}.

encrypt(_Side, <<>>, State)->
  {more, 1, State};
encrypt(_Side, Data, #state{key = Key, cipher_info = #cipher_info{iv_size = IvSize} = CipherInfo, encrypt_state = undefined} = State) ->
  IV = crypto:strong_rand_bytes(IvSize div 8),
  CipherState = cipher_init(CipherInfo, encrypt, Key, IV),

  {NewCipherState, Out} = cipher_update(CipherState, Data),

  {ok, <<IV/bytes, Out/bytes>>, empty, State#state{encrypt_state = NewCipherState}};
encrypt(_Side, Data, #state{encrypt_state = CipherState} = State) ->
  {NewCipherState, Out} = cipher_update(CipherState, Data),
  {ok, Out, empty, State#state{encrypt_state = NewCipherState}}.

decrypt(_Side, <<>>, State)->
  {more, 1, State};
decrypt(Side, Data, #state{key = Key, cipher_info = #cipher_info{iv_size = IvSize} = CipherInfo, decrypt_state = undefined} = State) ->
  try
    extract_iv(Data, IvSize)
  of
    {IV, RemainData} ->
      CipherState = cipher_init(CipherInfo, decrypt, Key, IV),
      decrypt(Side, RemainData, State#state{decrypt_state = CipherState})
  catch
    error:_ -> {more, IvSize div 8, State}
  end;
decrypt(_Side, Data, #state{decrypt_state = CipherState} = State) ->
  {NewCipherState, Out} = cipher_update(CipherState, Data),
  {ok, Out, empty, State#state{decrypt_state = NewCipherState}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

extract_iv(Data, IvSize) ->
  <<IV:(IvSize)/bitstring, RemainData/bitstring>> = Data,
  {IV, RemainData}.


ciphers("aes-128-cfb") -> #cipher_info{name = "aes-128-cfb", cipher_type = cfb, cipher = aes_cfb128, iv_size = 128, key_size = 128};
ciphers("aes-192-cfb") -> #cipher_info{name = "aes-192-cfb", cipher_type = cfb, cipher = aes_cfb128, iv_size = 128, key_size = 192};
ciphers("aes-256-cfb") -> #cipher_info{name = "aes-256-cfb", cipher_type = cfb, cipher = aes_cfb128, iv_size = 128, key_size = 256};
ciphers("aes-128-ctr") -> #cipher_info{name = "aes-128-ctr", cipher_type = stream, cipher = aes_ctr, iv_size = 128, key_size = 128};
ciphers("aes-192-ctr") -> #cipher_info{name = "aes-192-ctr", cipher_type = stream, cipher = aes_ctr, iv_size = 128, key_size = 192};
ciphers("aes-256-ctr") -> #cipher_info{name = "aes-256-ctr", cipher_type = stream, cipher = aes_ctr, iv_size = 128, key_size = 256};
ciphers("bf-cfb") -> #cipher_info{name = "bf-cfb", cipher_type = cfb, cipher = blowfish_cfb64, iv_size = 64, key_size = 128}.

cipher_init(#cipher_info{cipher_type = stream, cipher = Cipher} = Info, Type, Key, IV) ->
  #cipher_state{context = crypto:stream_init(Cipher, Key, IV), cipher_info = Info, type = Type};
cipher_init(#cipher_info{cipher_type = cfb, cipher = Cipher, iv_size = BlockSize} = Info, Type, Key, IV) ->
  #cipher_state{context = cfb_stream:init_context(Cipher, Type, BlockSize, Key, IV), cipher_info = Info, type = Type}.

cipher_update(#cipher_state{context = Context, cipher_info = #cipher_info{cipher_type = stream}, type = encrypt} = State, Data) ->
  {NewContext, Out} = crypto:stream_encrypt(Context, Data),
  {State#cipher_state{context = NewContext}, Out};
cipher_update(#cipher_state{context = Context, cipher_info = #cipher_info{cipher_type = stream}, type = decrypt} = State, Data) ->
  {NewContext, Out} = crypto:stream_decrypt(Context, Data),
  {State#cipher_state{context = NewContext}, Out};
cipher_update(#cipher_state{context = Context, cipher_info = #cipher_info{cipher_type = cfb}} = State, Data) ->
  {NewContext, Out} = cfb_stream:cipher_update(Context, Data),
  {State#cipher_state{context = NewContext}, Out}.
