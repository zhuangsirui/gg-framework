-module(database_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Db = {
    database,
    {database, start_link, []},
    permanent,
    2000,
    worker,
    [database]
  },
  Children = [Db],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

