-module(gg_framework_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

init([]) ->
  AcceptorSup = {
    gg_acceptor_sup,
    {gg_acceptor_sup, start_link, []},
    permanent,
    2000,
    supervisor,
    [gg_acceptor_sup]
  },
  ServerSup = {
    gg_server_sup,
    {gg_server_sup, start_link, []},
    permanent,
    2000,
    supervisor,
    [gg_server_sup]
  },
  DatabaseSup = {
    database_sup,
    {database_sup, start_link, []},
    permanent,
    2000,
    supervisor,
    [database_sup]
  },
  ProjectSup = {
    project_sup,
    {project_sup, start_link, []},
    permanent,
    2000,
    supervisor,
    [project_sup]
  },
  Children = [AcceptorSup, ServerSup, DatabaseSup, ProjectSup],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
