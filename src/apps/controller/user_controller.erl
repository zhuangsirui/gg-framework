-module(user_controller).

-behaviour(gg_controller_behaviour).

%% Callbacks for gg_controller_behaviour
-export([
  dispatch_filter/2
]).

%% APIs
-export([
  user_register/1,
  user_login/1,
  user_logout/1,
  user_init/1,
  user_info/1
]).

dispatch_filter(Action, State) ->
  case Action of
    user_register ->
      {ok, user_register, State};
    user_login ->
      {ok, user_login, State};
    user_info ->
      {transmit, user_info, State};
    %user_init ->
      %{transmit, user_init, State};
    _Other ->
      {deny, normal, State}
  end.

user_register([Username, Udid]) ->
  Res = {ok, {Username, Udid}},
  case Res of
    {ok, {Username, Udid}} ->
      io:format("~p, welcome to register my world!~n", [Username]),
      user_login([Username, Udid]);
    _ ->
      {error, ?MODULE}
  end.

user_login([Username, Udid]) ->
  Res = {ok, {Username, Udid}},
  case Res of
    {ok, {Username, Udid}} ->
      sync_all_to_mem(Udid),
      io:format("~p, welcome to my world!~n", [Username]),
      {ok, SocketUserSup} = gg_config:get_socket_user_sup(),
      {ok, UserPid} = SocketUserSup:start_child([self(), Udid, 1]),
      gen_server:cast(UserPid, {call, {?MODULE, user_init, [Udid]}}),
      io:format("[1] socket_user PID: ~p~n", [UserPid]),
      CallBack = [
        {ack, {100, [Username, Udid]}},
        {transmit_pid, UserPid}
      ],
      io:format("[1] socket_user PID: ~p~n", [UserPid]),
      {ok, CallBack};
    {error, Reason} ->
      {error, Reason}
  end.

user_init([Udid]) ->
  io:format("=================SELF PID:~p=================~n", [self()]),
  T = ets:new(?MODULE, []),
  ets:insert(T, {username, "zhuangsirui"}),
  ets:insert(T, {udid, Udid}),
  CallBack = [],
  {ok, CallBack}.

user_info([]) ->
  [{username, Username} | _] = ets:lookup(?MODULE, username),
  [{udid, Udid} | _] = ets:lookup(?MODULE, udid),
  CallBack = [
    {ack, {100, [Username, Udid]}}
  ],
  {ok, CallBack}.

user_logout([]) ->
  CallBack = [
    {ack, {100, ["username for test", "udid for test"]}},
    {logout}
  ],
  {ok, CallBack}.

sync_all_to_mem(Udid) ->
  {ok, Udid}.
