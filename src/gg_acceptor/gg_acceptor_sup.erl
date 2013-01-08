-module(gg_acceptor_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0
]).

%% Supervisor callbacks
-export([
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Acceptor = gg_config:get_acceptor(),
  AcceptorConf = {
    Acceptor,
    {Acceptor, start_link, [self()]},
    permanent,
    2000,
    worker,
    [Acceptor]
  },
  Children = [AcceptorConf],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
