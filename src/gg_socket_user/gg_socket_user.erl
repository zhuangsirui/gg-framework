%% -------------------------------------------------------------------
%% @author Sirius Zhuang <siriusibunny@gmail.com>
%%   [http://github.com/siriuszhuang]
%% @version 0.0.1
%% @doc A gen server for socket user.
%% -------------------------------------------------------------------
-module(gg_socket_user).

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  stop/1
  %set_socket/2, % 用server的trasmit_pid替代发送消息
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
  parent_pid = null,
  user_id = null,
  session_code = 0
}).

%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------
start_link([ParentPid, UserId, SessionCode]) ->
  gen_server:start_link(?MODULE, [ParentPid, UserId, SessionCode], []).

stop(Ref) ->
  gen_server:cast(Ref, stop).

%% -------------------------------------------------------------------
%% Gen server callbacks
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc Init socket user progress
%% -------------------------------------------------------------------
init([ParentPid, UserId, SessionCode]) ->
  {ok, #state{
    parent_pid = ParentPid,
    user_id = UserId,
    session_code = SessionCode
  }}.

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
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({call, {Controller, Action, Params}}, State) ->
  Res = Controller:Action(Params),
  case Res of
    {ok, InfoList} ->
      {_Sign, _State} = dispatch_callback(InfoList, noreply, State),
      {noreply, State};
    {deny, Reply} ->
      Reply;
    _Other ->
      % 异常处理
      {stop, unknown_info}
  end;
handle_cast(_Event, _State) ->
  {stop, unknown_cast}.

dispatch_callback([], Sign, State) ->
  {Sign, State};
dispatch_callback([H | T], Sign, State) ->
  case H of
    {ack, {AckCode, Params}} ->
      NewState = State,
      NewSign = Sign,
      gen_server:cast(State#state.parent_pid, {send, {AckCode, Params}});
    {sign, NewSign} ->
      NewState = State,
      ignore;
    %{transmit_pid, Pid} ->
      %NewSign = Sign,
      %NewState = #state{
        %socket = State#state.socket,
        %transmit_pid = Pid
      %};
    _Other ->
      NewSign = Sign,
      NewState = State,
      ignore
  end,
  dispatch_callback(T, NewSign, NewState).

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
handle_info({tcp, _Socket, _Bin}, State) ->
  {noreply, State};
handle_info(_Info, _State) ->
  {stop, unknown_info}.
