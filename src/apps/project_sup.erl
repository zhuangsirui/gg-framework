-module(project_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

init([]) ->
  SocketUserSup = {
    gg_socket_user_sup,
    {gg_socket_user_sup, start_link, []},
    permanent,
    2000,
    supervisor,
    [gg_socket_user_sup]
  },
  Children = [SocketUserSup],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
