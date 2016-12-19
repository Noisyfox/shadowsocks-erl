%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十二月 2016 18:23
%%%-------------------------------------------------------------------
-module(gen_protocol).
-author("Noisyfox").

%% API
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  % Side :: client OR server
  % Remain Data :: bytes() OR empty
  [{init, 2}, % ? -> State
    {close, 1}, % State -> {ok} or {error, Reason}
    {encapsule, 3}, % Side, Data, State -> {ok, New Data, Remain Data, New State} OR {error, Reason, New State} OR {more, Length, New State}
    {decapsule, 3} % Side, Data, State -> {ok, New Data, Remain Data, New State} OR {error, Reason, New State} OR {more, Length, New State}
  ];
behaviour_info(_) ->
  undefined.
