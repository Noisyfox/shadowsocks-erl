%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十二月 2016 18:22
%%%-------------------------------------------------------------------
-module(gen_obfs).
-author("Noisyfox").

-callback init(Side :: client | server) -> {ok, State :: term()} | {error, Reason :: term()}.

-callback close(Side :: client | server, State :: term()) -> {ok} | {error, Reason :: term()}.

-callback obfuscate(Side :: client | server, Data :: binary(), State :: term()) ->
  {ok, NewData :: binary(), RemainData :: binary() | empty, NewState :: term()}|
  {error, Reason :: term(), NewState :: term()}|
  {more, Length :: non_neg_integer(), NewState :: term()}.

-callback clarify(Side :: client | server, Data :: binary(), State :: term()) ->
  {ok, NewData :: binary(), RemainData :: binary() | empty, NewState :: term()}|
  {error, Reason :: term(), NewState :: term()}|
  {more, Length :: non_neg_integer(), NewState :: term()}.
