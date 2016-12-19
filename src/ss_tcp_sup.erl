%%%-------------------------------------------------------------------
%%% @author Noisyfox
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十二月 2016 23:42
%%%-------------------------------------------------------------------
-module(ss_tcp_sup).
-author("Noisyfox").

-behaviour(supervisor).

%% API
-export([start_link/2]).

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
-spec(start_link(LSock :: term(), Config :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(LSock, Config) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock, Config]).

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
init([LSock, Config]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 0, % for debug only
    period => 1 % for debug only
  },

  ListenSrv = #{
    id => 'TCP Listener',
    start => {ss_tcp_listen_server, start_link, [LSock, Config]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [ss_tcp_listen_server]
  },

  RelaySup = #{
    id => 'TCP Relay Supervisor',
    start => {ss_tcp_relay_sup, start_link, [Config]},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [ss_tcp_relay_sup]
  },

  {ok, {SupFlags, [ListenSrv, RelaySup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
