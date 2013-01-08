%% -------------------------------------------------------------------
%% @author Sirius Zhuang <siriusibunny@gmail.com>
%%   [http://github.com/siriuszhuang]
%% @version 0.0.1
%% @doc Config control module by analysis file
%% include/config/gg_config.hrl. You can change the default module by
%% edit the gg_config.hrl file.
%% @end
%% -------------------------------------------------------------------
-module(gg_config).

-export([
  get_analysor/0,
  get_server/0,
  get_acceptor/0,
  get_acknowledger/0,
  get_acceptor_port/0,
  get_db_options/0,
  get_socket_user/0,
  get_socket_user_sup/0,
  get_path/1
]).

-include_lib("config/gg_config.hrl").

%% -------------------------------------------------------------------
%% @doc get socket user module
%% -------------------------------------------------------------------
-spec get_socket_user() -> {ok, Module::atom()}.
get_socket_user() ->
  get_app_component(socket_user, app_components()).

%% -------------------------------------------------------------------
%% @doc get socket user sup module
%% -------------------------------------------------------------------
-spec get_socket_user_sup() -> {ok, Module::atom()}.
get_socket_user_sup() ->
  get_app_component(socket_user_sup, app_components()).

get_app_component(_ComponentName, []) ->
  {error, not_found};
get_app_component(ComponentName, [H, T]) ->
  case H of
    {_ComponentName, Module} ->
      {ok, Module};
    _Other ->
      get_app_component(ComponentName, T)
  end.

%% -------------------------------------------------------------------
%% @doc Get analysor for framework
%% -------------------------------------------------------------------
-spec get_analysor() -> Module::atom().
get_analysor() ->
  case get_component_config(analysor) of
    {ok, Module} ->
      Module;
    {error, not_found} ->
      gg_analysor
  end.

%% -------------------------------------------------------------------
%% @doc Get server for framework
%% -------------------------------------------------------------------
-spec get_server() -> Module::atom().
get_server() ->
  case get_component_config(server) of
    {ok, Module} ->
      Module;
    {error, not_found} ->
      gg_server
  end.

%% -------------------------------------------------------------------
%% @doc Get acceptor for framework
%% -------------------------------------------------------------------
-spec get_acceptor() -> Module::atom().
get_acceptor() ->
  case get_component_config(acceptor) of
    {ok, Module} ->
      Module;
    {error, not_found} ->
      gg_acceptor
  end.

%% -------------------------------------------------------------------
%% @doc Get acceptor listen port
%% -------------------------------------------------------------------
-spec get_acceptor_port() -> Port::integer().
get_acceptor_port() ->
  get_connection_config(acceptor_port).

get_connection_config(Type) ->
  get_connection_config(Type, connection_configs()).
get_connection_config(_Type, []) ->
  {error, config_undefined};
get_connection_config(Type, [H | T]) ->
  case H of
    {Type, Value} ->
      Value;
    _Other ->
      get_connection_config(Type, T)
  end.

%% -------------------------------------------------------------------
%% @doc Get acknowledger for framework
%% -------------------------------------------------------------------
-spec get_acknowledger() -> Module::atom().
get_acknowledger() ->
  case get_component_config(acknowledger) of
    {ok, Module} ->
      Module;
    {error, not_found} ->
      gg_acknowledger
  end.

%% -------------------------------------------------------------------
%% @doc Get database options
%% -------------------------------------------------------------------
-spec get_db_options() -> Options::list().
get_db_options() ->
  db_configs().

get_path(Type) ->
  get_path_config(Type, path_configs()).
get_path_config(_Type, []) ->
  {error, not_found};
get_path_config(Type, [H | T]) ->
  case H of
    {Type, Path} ->
      {ok, Path};
    _ ->
      get_path_config(Type, T)
  end.

get_component_config(ComponentName) ->
  get_component_config(ComponentName, component_configs()).
get_component_config(_ComponentName, []) ->
  {error, not_found};
get_component_config(ComponentName, [H | T]) ->
  case H of
    {ComponentName, Module} ->
      {ok, Module};
    _ ->
      get_component_config(ComponentName, T)
  end.
