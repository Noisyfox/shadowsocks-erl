-module(ss_tcp_listen_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {
  lsock,
  config
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

start_link(LSock, Config) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [LSock, Config], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([LSock, Config]) ->
  {ok, #state{lsock = LSock, config = Config}, 0}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state{lsock = LSock, config = Config} = State) ->
  {ok, ClientSocket} = gen_tcp:accept(LSock),

  Pid =
  case ss_tcp_relay_sup:start_relay(ClientSocket, Config) of
    {ok, undefined} -> undefined;
    {ok, undefined, _} -> undefined;
    {ok, P} -> P;
    {ok, P, _} -> P
  end, % TODO: handle return
  gen_tcp:controlling_process(ClientSocket, Pid),

  {noreply, State, 0}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

