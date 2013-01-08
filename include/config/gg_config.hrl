%% -------------------------------------------------------------------
%% @doc config the component module.
%% -------------------------------------------------------------------
component_configs() ->
  [
    {analysor, gg_analysor},
    {server, gg_server},
    {acceptor, gg_acceptor},
    {acknowledger, gg_acknowledger}
  ].

%% -------------------------------------------------------------------
%% @doc config the path env.
%% -------------------------------------------------------------------
path_configs() ->
  AppRoot = "/Users/zhuangsirui/Projects/erlang/gg-framework/",
  [
    {model, AppRoot ++ "src/apps/model/"},
    {controller, AppRoot ++ "src/apps/controller/"}
  ].

%% -------------------------------------------------------------------
%% @doc config the app port etc.
%% -------------------------------------------------------------------
connection_configs() ->
  [
    {acceptor_port, 1266}
  ].

%% -------------------------------------------------------------------
%% @doc config the database
%% -------------------------------------------------------------------
db_configs() ->
  [
    {adapter, mysql},
    {db_host, "localhost"},
    {db_port, 3306},
    {db_username, "root"},
    {db_passwrod, ""},
    {db_database, "test"},
    {cache_enable, false}
  ].

%% -------------------------------------------------------------------
%% @doc application components
%% -------------------------------------------------------------------
app_components() ->
  [
    {socket_user_sup, gg_socket_user_sup},
    {socket_user, gg_socket_user}
  ].
