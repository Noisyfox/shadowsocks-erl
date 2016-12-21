%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十二月 2016 0:38
%%%-------------------------------------------------------------------
-module(protocol_plain).
-author("Noisyfox").

-behavior(gen_protocol).

%% API
-export([init/0, close/1, encapsule/3, decapsule/3]).


init() ->
  {ok, {}}.

close(_State) ->
  {ok}.

encapsule(_Side, Data, State) ->
  {ok, Data, empty, State}.

decapsule(_Side, Data, State) ->
  {ok, Data, empty, State}.