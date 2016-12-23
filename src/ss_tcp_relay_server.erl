-module(ss_tcp_relay_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(TIMEOUT, 10000).

-record(net_buffer, {
  remain_obfs = <<>>,
  remain_cipher = <<>>,
  remain_protocol = <<>>,
  remain_proxy = <<>>,

  require_obfs = infinity,
  require_cipher = infinity,
  require_protocol = infinity,
  require_proxy = infinity
}).

-record(state, {
  config,

  client_sock,
  target_sock = undefined,

  from_stage = init, % from_stage = init | obfs | cipher | protocol | proxy
  to_stage = init, % to_stage = init | obfs | cipher | protocol | proxy

  module_obfs = obfs_plain,
  module_cipher = cipher_aes,
  module_protocol = protocol_plain,

  state_obfs,
  state_cipher,
  state_protocol,

  buffer_from = #net_buffer{},
  buffer_to = #net_buffer{}
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
handle_info(timeout, #state{from_stage = init, client_sock = ClientSocket} = State) ->

  #state{
    module_cipher = ModuleCipher,
    module_obfs = ModuleObfs,
    module_protocol = ModuleProtocol
  } = State,

  {ok, ObfsState} = ModuleCipher:init(server),
  {ok, CipherState} = ModuleObfs:init(server),
  {ok, ProtocolState} = ModuleProtocol:init(server),

  inet:setopts(ClientSocket, [{active, once}]),

  {noreply, State#state{
    from_stage = obfs,
    state_obfs = ObfsState,
    state_cipher = CipherState,
    state_protocol = ProtocolState
  }, ?TIMEOUT};

% real time out
handle_info(timeout, State) ->
  {noreply, State};

%% Receive from client
handle_info({tcp, ClientSocket, Data}, #state{client_sock = ClientSocket} = State) ->
  io:format("Recv from client: ~w~n", [Data]),
  inet:setopts(ClientSocket, [{active, once}]),
  {noreply, State};

%% Receive from target
handle_info({tcp, TargetSocket, Data}, #state{target_sock = TargetSocket} = State) ->
  io:format("Recv from target: ~w~n", [Data]),
  inet:setopts(TargetSocket, [{active, once}]),
  {noreply, State};

%% Client socket closed
handle_info({tcp_closed, ClientSocket}, #state{client_sock = ClientSocket} = State) ->
  io:format("Client socket closed~n", []),
  {noreply, State};

%% Target socket closed
handle_info({tcp_closed, TargetSocket}, #state{target_sock = TargetSocket} = State) ->
  io:format("Target socket closed~n", []),
  {noreply, State};

handle_info(Info, #state{client_sock = ClientSocket} = State) ->
  io:format("~w", [Info]),
  inet:setopts(ClientSocket, [{active, once}]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


next_stage(init) -> obfs;
next_stage(obfs) -> cipher;
next_stage(cipher) -> protocol;
next_stage(protocol) -> proxy;
next_stage(proxy) -> proxy.
