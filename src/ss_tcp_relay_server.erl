-module(ss_tcp_relay_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(TIMEOUT, 10000).

-record(net_buffer, {
  remain = empty,
  require = infinity
}).

-record(stage_state, {
  state,
  buffer_from = #net_buffer{},
  buffer_to = #net_buffer{}
}).

-record(state, {
  config,

  client_sock,
  target_sock = undefined,

  client_stage = init, % client_stage = init | running | closed
  target_stage = init, % target_stage = init | running | closed

  module_obfs = obfs_plain,
  module_cipher = cipher_aes,
  module_protocol = protocol_plain,

  stage_obfs = #stage_state{},
  stage_cipher = #stage_state{},
  stage_protocol = #stage_state{}
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ClientSock, Config) ->
  gen_server:start_link(?MODULE, [ClientSock, Config], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ClientSock, Config]) ->
  {ok, #state{config = Config, client_sock = ClientSock}, 0}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

% init trigger
handle_info(timeout, #state{client_stage = init, client_sock = ClientSocket} = State) ->

  #state{
    module_cipher = ModuleCipher,
    module_obfs = ModuleObfs,
    module_protocol = ModuleProtocol,
    stage_cipher = StageCipher,
    stage_obfs = StageObfs,
    stage_protocol = StageProtocol
  } = State,

  {ok, ObfsState} = ModuleObfs:init(server),
  {ok, CipherState} = ModuleCipher:init(server),
  {ok, ProtocolState} = ModuleProtocol:init(server),

  inet:setopts(ClientSocket, [{active, once}]),

  {noreply, State#state{
    client_stage = running,
    stage_obfs = StageObfs#stage_state{state = ObfsState},
    stage_cipher = StageCipher#stage_state{state = CipherState},
    stage_protocol = StageProtocol#stage_state{state = ProtocolState}
  }, ?TIMEOUT};

% real time out
handle_info(timeout, State) ->
  {noreply, State};

%% Receive from client
handle_info({tcp, ClientSocket, Data}, #state{client_sock = ClientSocket} = State) ->
  % io:format("Recv from client: ~w~n", [Data]),

  Result = run_stage(Data, from, State),

  % io:format("Result: ~w~n", [Result]),

  case Result of
    {ok, ProxyData, NewState} ->
      {ok, ReturnState} = handle_proxy_send(ProxyData, NewState),

      inet:setopts(ClientSocket, [{active, once}]),
      {noreply, ReturnState};
    {more, NewState} ->
      inet:setopts(ClientSocket, [{active, once}]),
      {noreply, NewState};
    {error, _Reason, NewState} ->
      {noreply, NewState}
  end;

%% Receive from target
handle_info({tcp, TargetSocket, Data}, #state{target_sock = TargetSocket} = State) ->
  % io:format("Recv from target: ~w~n", [Data]),

  Result = run_stage(Data, to, State),

  % io:format("Result: ~w~n", [Result]),

  case Result of
    {ok, ReplyData, NewState} ->
      {ok, ReplyState} = handle_reply(ReplyData, NewState),

      inet:setopts(TargetSocket, [{active, once}]),
      {noreply, ReplyState};
    {more, NewState} ->
      inet:setopts(TargetSocket, [{active, once}]),
      {noreply, NewState};
    {error, _Reason, NewState} ->
      {noreply, NewState}
  end;

handle_info({tcp_closed, ClientSocket}, #state{client_sock = ClientSocket} = State) ->
  handle_tcp_closed(ClientSocket, State);
handle_info({tcp_closed, TargetSocket}, #state{target_sock = TargetSocket} = State) ->
  handle_tcp_closed(TargetSocket, State);
handle_info({tcp_closed, ClientSocket, _}, #state{client_sock = ClientSocket} = State) ->
  handle_tcp_closed(ClientSocket, State);
handle_info({tcp_closed, TargetSocket, _}, #state{target_sock = TargetSocket} = State) ->
  handle_tcp_closed(TargetSocket, State);

handle_info(Info, #state{client_sock = ClientSocket} = State) ->
  io:format("~w~n", [Info]),
  inet:setopts(ClientSocket, [{active, once}]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

build_input(#net_buffer{remain = empty, require = _Require}, NewData) ->
  NewData;
build_input(#net_buffer{remain = RemainData, require = _Require}, NewData) ->
  <<RemainData/binary, NewData/binary>>.

stage_func(obfs, from) -> obfuscate;
stage_func(obfs, to) -> clarify;
stage_func(cipher, from) -> encrypt;
stage_func(cipher, to) -> decrypt;
stage_func(protocol, from) -> encapsule;
stage_func(protocol, to) -> decapsule.

next_stage(obfs, from) -> cipher;
next_stage(obfs, to) -> fin;
next_stage(cipher, from) -> protocol;
next_stage(cipher, to) -> obfs;
next_stage(protocol, from) -> fin;
next_stage(protocol, to) -> cipher.

run_stage(NewData, from, State) ->
  run_stage(obfs, NewData, from, State);
run_stage(NewData, to, State) ->
  run_stage(protocol, NewData, to, State).

run_stage(fin, NewData, _, State) ->
  {ok, NewData, State};
run_stage(obfs = Stage, NewData, Direction, #state{module_obfs = Module, stage_obfs = StageState} = State) ->
  case run_stage(Module, stage_func(Stage, Direction), NewData, Direction, StageState) of
    {ok, OutData, NewStageState} ->
      run_stage(next_stage(Stage, Direction), OutData, Direction, State#state{stage_obfs = NewStageState});
    {more, NewStageState} ->
      {more, State#state{stage_obfs = NewStageState}};
    {error, Reason, NewStageState} ->
      {error, Reason, State#state{stage_obfs = NewStageState}}
  end;
run_stage(cipher = Stage, NewData, Direction, #state{module_cipher = Module, stage_cipher = StageState} = State) ->
  case run_stage(Module, stage_func(Stage, Direction), NewData, Direction, StageState) of
    {ok, OutData, NewStageState} ->
      run_stage(next_stage(Stage, Direction), OutData, Direction, State#state{stage_cipher = NewStageState});
    {more, NewStageState} ->
      {more, State#state{stage_cipher = NewStageState}};
    {error, Reason, NewStageState} ->
      {error, Reason, State#state{stage_cipher = NewStageState}}
  end;
run_stage(protocol = Stage, NewData, Direction, #state{module_protocol = Module, stage_protocol = StageState} = State) ->
  case run_stage(Module, stage_func(Stage, Direction), NewData, Direction, StageState) of
    {ok, OutData, NewStageState} ->
      run_stage(next_stage(Stage, Direction), OutData, Direction, State#state{stage_protocol = NewStageState});
    {more, NewStageState} ->
      {more, State#state{stage_protocol = NewStageState}};
    {error, Reason, NewStageState} ->
      {error, Reason, State#state{stage_protocol = NewStageState}}
  end.

run_stage(Module, Function, NewData, from, #stage_state{state = State, buffer_from = NetBuffer} = StageState) ->
  case run_stage2(Module, Function, NewData, NetBuffer, State) of
    {ok, OutData, NewNetBuffer, NewState} ->
      {ok, OutData, StageState#stage_state{state = NewState, buffer_from = NewNetBuffer}};
    {more, NewNetBuffer, NewState} ->
      {more, StageState#stage_state{state = NewState, buffer_from = NewNetBuffer}};
    {error, Reason, NewState} ->
      {error, Reason, StageState#stage_state{state = NewState}}
  end;
run_stage(Module, Function, NewData, to, #stage_state{state = State, buffer_to = NetBuffer} = StageState) ->
  case run_stage2(Module, Function, NewData, NetBuffer, State) of
    {ok, OutData, NewNetBuffer, NewState} ->
      {ok, OutData, StageState#stage_state{state = NewState, buffer_to = NewNetBuffer}};
    {more, NewNetBuffer, NewState} ->
      {more, StageState#stage_state{state = NewState, buffer_to = NewNetBuffer}};
    {error, Reason, NewState} ->
      {error, Reason, StageState#stage_state{state = NewState}}
  end.

run_stage2(Module, Function, NewData, NetBuffer, State) ->
  InData = build_input(NetBuffer, NewData),

  case Module:Function(server, InData, State) of
    {ok, OutData, RemainData, NewState} ->
      {ok, OutData, #net_buffer{remain = RemainData, require = infinity}, NewState};
    {more, Length, NewState} ->
      {more, #net_buffer{remain = InData, require = Length}, NewState};
    {error, _, _} = Result ->
      Result
  end.

handle_proxy_send(Data, #state{target_sock = undefined} = State) ->
  {Addr, Port, Remaining} = parse_address(Data),

  OptsTCP = [binary, {active, once}, {nodelay, true}, {packet, raw}],

  {ok, Socket} = gen_tcp:connect(Addr, Port, OptsTCP),

  handle_proxy_send(Remaining, State#state{target_sock = Socket, target_stage = running});
handle_proxy_send(_, #state{target_sock = closed} = State)->
  {ok, State};
handle_proxy_send(Data, #state{target_sock = TargetSock} = State) ->
  case gen_tcp:send(TargetSock, Data) of
    ok -> {ok, State};
    {error, closed} ->
      gen_server:cast(self(), {tcp_closed, TargetSock}),
      {ok, State#state{target_stage = closed}}
  end.

handle_reply(_, #state{client_stage = closed} = State)->
  {ok, State};
handle_reply(Data, #state{client_sock = Socket} = State) ->
  case gen_tcp:send(Socket, Data) of
    ok -> {ok, State};
    {error, closed} ->
      gen_server:cast(self(), {tcp_closed, Socket}),
      {ok, State#state{client_stage = closed}}
  end.

parse_address(<<1, Addr:4/bytes, Port:16/unsigned, Remaining/bytes>>) -> % ATYP = 1
  {binary_utils:bin_to_ipv4(Addr), Port, Remaining};
parse_address(<<3, Len/unsigned, Addr:(Len)/bytes, Port:16/unsigned, Remaining/bytes>>) -> % ATYP = 3
  {binary_to_list(Addr), Port, Remaining};
parse_address(<<4, Addr:16/bytes, Port:16/unsigned, Remaining/bytes>>) -> % ATYP = 4
  {binary_utils:bin_to_ipv6(Addr), Port, Remaining}.

handle_tcp_closed(ClientSocket, #state{client_sock = ClientSocket, target_stage = TargetStage} = State) ->
  io:format("Client socket closed~n", []),

  case TargetStage of
    running -> {noreply, State#state{client_stage = closed}};
    _ -> {stop, normal, State#state{client_stage = closed}}
  end;
handle_tcp_closed(TargetSocket, #state{target_sock = TargetSocket, client_stage = ClientStage} = State) ->
  io:format("Target socket closed~n", []),

  case ClientStage of
    running -> {noreply, State#state{target_sock = closed}};
    _ -> {stop, normal, State#state{target_sock = closed}}
  end.

