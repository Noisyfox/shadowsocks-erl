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

%% API
-export([init/0, close/1, encrypt/3, decrypt/3]).


init() ->
  {ok, {}}.

close(State) ->
  {ok}.

encrypt(Side, Data, State) ->
  erlang:error(not_implemented).

decrypt(Side, Data, State) ->
  erlang:error(not_implemented).