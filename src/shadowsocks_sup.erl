-module(shadowsocks_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSockTCP, LSockUDP, Config) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [LSockTCP, LSockUDP, Config]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LSockTCP, _LSockUDP, Config]) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 0, % for debug only
    period => 1 % for debug only
  },

  TCPSup = #{
    id => 'TCP Supervisor',
    start => {ss_tcp_sup, start_link, [LSockTCP, Config]},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [ss_tcp_sup]
  },

  {ok, {SupFlags, [TCPSup]}}.

