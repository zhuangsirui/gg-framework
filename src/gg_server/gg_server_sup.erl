-module(gg_server_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_child/0
]).

%% Supervisor callbacks
-export([
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
  supervisor:start_child(?MODULE, []).

init([]) ->
  Server = gg_config:get_server(),
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
