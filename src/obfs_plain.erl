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
-export([init/0, close/1, obfuscate/3, clarify/3]).


init() ->
  {ok, {}}.

close(_State) ->
  {ok}.

obfuscate(_Side, Data, State) ->
  {ok, Data, empty, State}.

clarify(_Side, Data, State) ->
  {ok, Data, empty, State}.