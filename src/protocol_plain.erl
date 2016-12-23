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
-export([init/1, close/2, encapsule/3, decapsule/3]).


init(_Side) ->
  {ok, {}}.

close(_Side, _State) ->
  {ok}.

encapsule(_Side, Data, State) ->
  {ok, Data, empty, State}.

decapsule(_Side, Data, State) ->
  {ok, Data, empty, State}.