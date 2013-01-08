%% -------------------------------------------------------------------
%% @author Sirius Zhuang <siriusibunny@gmail.com>
%%   [http://github.com/siriuszhuang]
%% @version 0.0.1
%% @doc The default server for gg_framework. Call decoder to decode,
%% dispatch request and acknowledge to client.
%% @end
%% -------------------------------------------------------------------
-module(gg_server).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  stop/1,
  set_socket/2,
  analysis_protocol/1
]).

%% Gen server callbacks
-export([
  init/1,
  handle_cast/2,
  handle_call/3,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  socket = null,
  transmit_pid = null
}).

%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------
start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop(Ref) ->
  gen_server:cast(Ref, stop).

set_socket(ServerPid, Socket) ->
  gen_server:cast(ServerPid, {set_socket, Socket}).

%% -------------------------------------------------------------------
%% Gen server callbacks
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc Init server
%% -------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%% -------------------------------------------------------------------
%% @doc Handle server when it down.
%% -------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%% -------------------------------------------------------------------
%% @doc Update code change.
%% -------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% -------------------------------------------------------------------
%% @doc Get cast sign
%% -------------------------------------------------------------------
handle_cast({set_socket, Socket}, State) ->
  io:format("handle_cast -> set_socket~n"),
  ok = inet:setopts(Socket, [
    binary,
    {active, once},
    {packet, 2}
  ]),
  {noreply, State#state{socket = Socket}};
handle_cast({send, {AckCode, Params}}, State) ->
  send(State#state.socket, {AckCode, Params}),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Event, _State) ->
  {stop, unknown_cast}.

%% -------------------------------------------------------------------
%% @doc Get call sign
%% -------------------------------------------------------------------
handle_call(_Event, _From, _State) ->
  {stop, unknown_call}.

%% -------------------------------------------------------------------
%% @doc Get info sign
%% -------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info({tcp, Socket, Bin}, State) ->
  inet:setopts(Socket, [{active, once}]),
  RequestInfo = analysis_protocol(Bin),
  io:format("$$$ Analysis protocol result: ~p~n", [RequestInfo]),
  {ok, {
    {controller, Controller},
    {action, Action},
    {params, Params}
  }} = RequestInfo,
  Result = dispatch(Controller, Action, Params, State),
  case Result of
    {ok, InfoList} ->
      {_Sign, _State} = dispatch_callback(InfoList, noreply, State);
    {deny, Reply} ->
      Reply;
    _Other ->
      % 异常处理
      {stop, unknown_info}
  end;
handle_info(_Info, _State) ->
  {stop, unknown_info}.

%% -------------------------------------------------------------------
%% @doc Callback after dispatch controller. Handle the action params.
%% Param `{ack, {AckCode, Params}}' to make an acknowledge for client,
%% param `{session_code, SessionCode}' to change the socket's session
%% code, param `{sign, Sign}' to change the gen_server's status, like
%% `noreply' or `stop'.
%%
%% Example: `[{ack, {100, [Username, Udid]}}]'
%% @end
%% -------------------------------------------------------------------
dispatch_callback([], Sign, State) ->
  {Sign, State};
dispatch_callback([H | T], Sign, State) ->
  case H of
    {ack, {AckCode, Params}} ->
      NewState = State,
      NewSign = Sign,
      send(State#state.socket, {AckCode, Params});
    {sign, NewSign} ->
      NewState = State,
      ignore;
    {transmit_pid, Pid} ->
      NewSign = Sign,
      NewState = #state{
        socket = State#state.socket,
        transmit_pid = Pid
      };
    _Other ->
      NewSign = Sign,
      NewState = State,
      ignore
  end,
  dispatch_callback(T, NewSign, NewState).

%% -------------------------------------------------------------------
%% @doc Send binary data to socket
%% -------------------------------------------------------------------
send(Socket, {AckCode, Params}) ->
  {ok, Data} = encode_for_ack(AckCode, Params),
  case gen_tcp:send(Socket, Data) of
    ok ->
      io:format("> Ack success!~n");
    _Other ->
      io:format("> Ack error!~n")
  end,
  ok.

%% -------------------------------------------------------------------
%% @doc Encode vars to binary for client.
%% -------------------------------------------------------------------
-spec encode_for_ack(AckCode::integer(), Params::list()) ->
  {ok, BinaryData::binary()}.
encode_for_ack(AckCode, Params) ->
  Ackor = gg_config:get_acknowledger(),
  case Ackor:acknowledge({AckCode, Params}) of
    {ok, BinaryData} ->
      {ok, BinaryData};
    {error, Reason} ->
      throw({error, "Encode for ack error!", Reason})
  end.

%% -------------------------------------------------------------------
%% @doc Dispatch params to action of controller.
%% -------------------------------------------------------------------
-spec dispatch(
  Controller::atom(),
  CallAction::atom(),
  Params::list(),
  CallState::tuple()
) -> {ok | transmit, Result::list()} | {deny, Reply::tuple()}.
dispatch(Controller, CallAction, Params, CallState) ->
  FilterResult = gg_controller_behaviour:before_dispatch(
    Controller,
    CallAction,
    CallState
  ),
  case FilterResult of
    {ok, Action, _State} ->
      call(Controller, Action, Params);
    {transmit, Action, _State} ->
      transmit_call(Controller, Action, Params, CallState);
    {deny, Sign, State} ->
      {deny, {Sign, State}}
  end.

%% -------------------------------------------------------------------
%% @doc Call controller and action at server process
%% -------------------------------------------------------------------
call(Controller, Action, Params) ->
  Controller:Action(Params).

%% -------------------------------------------------------------------
%% @doc Call controller and action at transmit process
%% -------------------------------------------------------------------
transmit_call(Controller, Action, Params, State) ->
  gen_server:cast(
    State#state.transmit_pid,
    {call, {Controller, Action, Params}}
  ),
  {ok, []}.

%% -------------------------------------------------------------------
%% @doc Analysis the data from client and decode the binary data to
%% erlang variable.
%% @end
%% -------------------------------------------------------------------
-spec analysis_protocol(RawData::binary()) ->
  {ok, Data::tuple()} | {error, Reason::atom()}.
analysis_protocol(<<CallCode:16, ErrorCode:16, Data/binary>>) ->
  io:format("$$$ Analysis protocol: action ~p~n", [CallCode]),
  Analysor = gg_config:get_analysor(),
  Analysor:analysis(CallCode, ErrorCode, Data).
