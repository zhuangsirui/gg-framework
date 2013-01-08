-module(database).

-export([
  start_link/0
]).

start_link() ->
  boss_db:start(gg_config:get_db_options()),
  boss_news:start().
