%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十二月 2016 22:03
%%%-------------------------------------------------------------------
-module(gen_cipher).
-author("Noisyfox").

-callback init() -> {ok, State :: term()} | {error, Reason :: term()}.

-callback close(State :: term()) -> {ok} | {error, Reason :: term()}.

-callback encrypt(Side :: client | server, Data :: binary(), State :: term()) ->
  {ok, NewData :: binary(), RemainData :: binary() | empty, NewState :: term()}|
  {error, Reason :: term(), NewState :: term()}|
  {more, Length :: non_neg_integer(), NewState :: term()}.

-callback decrypt(Side :: client | server, Data :: binary(), State :: term()) ->
  {ok, NewData :: binary(), RemainData :: binary() | empty, NewState :: term()}|
  {error, Reason :: term(), NewState :: term()}|
  {more, Length :: non_neg_integer(), NewState :: term()}.
