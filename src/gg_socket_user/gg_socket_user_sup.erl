-module(gg_socket_user_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_child/1
]).

%% Supervisor callbacks
-export([
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% -------------------------------------------------------------------
%% @doc Fork a socket player progress to handle player's request.
%% -------------------------------------------------------------------
start_child([ParentPid, UserId, SessionCode]) ->
  io:format("[1] ParentPid: ~p~n", [ParentPid]),
  io:format("[1] ParentPid: ~p~n", [UserId]),
  supervisor:start_child(?MODULE, [[ParentPid, UserId, SessionCode]]).

init([]) ->
  Server = gg_socket_user,
  ServerConf = {
    Server,
    {Server, start_link, []},
    temporary,
    brutal_kill,
    worker,
    [Server]
  },
  RestartStrategy = {simple_one_for_one, 0, 1},
  Children = [ServerConf],
  {ok, {RestartStrategy, Children}}.
