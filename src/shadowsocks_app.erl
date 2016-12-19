-module(shadowsocks_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % TODO: First load configs

    OptsTCP = [binary, {active, false}, {nodelay, true}, {packet, raw}, {backlog, 30}],
    {ok, LSockTCP} = gen_tcp:listen(6666, OptsTCP),
    shadowsocks_sup:start_link(LSockTCP, ok, ok).

stop(_State) ->
    ok.
