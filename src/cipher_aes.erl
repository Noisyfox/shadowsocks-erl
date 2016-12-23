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
-export([init/1, close/2, encrypt/3, decrypt/3]).


init(_Side) ->
  {ok, {}}.

close(_Side, _State) ->
  {ok}.

encrypt(_Side, Data, State) ->
  {ok, Data, empty, State}.

decrypt(_Side, Data, State) ->
  {ok, Data, empty, State}.