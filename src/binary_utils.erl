%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 十二月 2016 4:25
%%%-------------------------------------------------------------------
-module(binary_utils).
-author("Noisyfox").

%% API
-export([bin_to_ipv4/1, bin_to_ipv6/1, password_to_key/2]).

bin_to_ipv4(<<A, B, C, D>>) ->
  {A, B, C, D}.

bin_to_ipv6(<<A:16/unsigned, B:16/unsigned, C:16/unsigned, D:16/unsigned,
  E:16/unsigned, F:16/unsigned, G:16/unsigned, H:16/unsigned>>) ->
  {A, B, C, D, E, F, G, H}.


password_to_key(Psw, KeyLength) when is_integer(KeyLength) and (KeyLength > 0) ->
  password_to_key(Psw, KeyLength, [], []).

password_to_key(Psw, RemainLength, LastHash, Result) when (RemainLength =< 16) ->
  MD5 = crypto:hash(md5, [LastHash, Psw]),
  <<Append:RemainLength/bytes, _/bytes>> = MD5,
  [Result, Append];
password_to_key(Psw, RemainLength, LastHash, Result) ->
  MD5 = crypto:hash(md5, [LastHash, Psw]),
  password_to_key(Psw, RemainLength - 16, MD5, [Result, MD5]).
