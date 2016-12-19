%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 十二月 2016 0:12
%%%-------------------------------------------------------------------
-module(ss_tcp_relay_sup).
-author("Noisyfox").

-behaviour(supervisor).

%% API
-export([start_link/1, start_relay/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Config :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(_Config) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_relay(Sock, Config) ->
  supervisor:start_child(?SERVER, [Sock, Config]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 0, % for debug only
    period => 1 % for debug only
  },

  Relay = #{
    id => 'TCP Listener',
    start => {ss_tcp_relay_server, start_link, []},
    restart => temporary,
    shutdown => 500,
    type => worker,
    modules => [ss_tcp_relay_server]
  },

  {ok, {SupFlags, [Relay]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
