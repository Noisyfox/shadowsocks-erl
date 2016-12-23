%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十二月 2016 0:16
%%%-------------------------------------------------------------------
-module(obfs_plain).
-author("Noisyfox").

-behaviour(gen_obfs).

%% API
-export([init/1, close/2, obfuscate/3, clarify/3]).


init(_Side) ->
  {ok, {}}.

close(_Side, _State) ->
  {ok}.

obfuscate(_Side, Data, State) ->
  {ok, Data, empty, State}.

clarify(_Side, Data, State) ->
  {ok, Data, empty, State}.