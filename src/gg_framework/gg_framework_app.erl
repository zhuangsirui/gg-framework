-module(gg_framework_app).

-behaviour(application).

%% API
-export([
  start/0
]).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

start() ->
  application:start(gg_framework).

%% -------------------------------------------------------------------
%% Application callbacks
%% -------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  compile_model(),
  gg_framework_sup:start_link().

stop(_State) ->
  ok.

%% -------------------------------------------------------------------
%% @doc Compile model with boss_record_compiler
%% -------------------------------------------------------------------
compile_model() ->
  {ok, ModelPath} = gg_config:get_path(model),
  {ok, FileList} = file:list_dir(ModelPath),
  compile_model(FileList, ModelPath),
  ok.

compile_model([], _ModelPath) ->
  ok;
compile_model([F | T], ModelPath) ->
  case boss_record_compiler:compile(ModelPath ++ F) of
    {ok, _Module} ->
      compile_model(T, ModelPath);
    _Other ->
      throw({error, {?MODULE, F ++ " is not a erlang source file!"}})
  end.
