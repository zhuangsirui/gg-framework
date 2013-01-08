%% -------------------------------------------------------------------
%% @author Sirius Zhuang <siriusibunny@gmail.com>
%%   [http://github.com/siriuszhuang]
%% @version 0.0.1
%% @doc The default controller behaviour for gg_framework, define a
%% callback named dispatch_filter for filter dispatch.
%% @end
%% -------------------------------------------------------------------
-module(gg_controller_behaviour).

%% Behaviour callbacks
-export([behaviour_info/1]).

%% APIs
-export([
  before_dispatch/3,
  do_ack/2
]).

behaviour_info(callbacks) ->
  [
    {dispatch_filter, 2}
  ];
behaviour_info(_Other) ->
  undefined.

%% -------------------------------------------------------------------
%% @doc Call function dispatch_filter of controller, return the access
%% for the request.
%% @end
%% -------------------------------------------------------------------
-spec before_dispatch(
  Controller::atom(),
  Action::atom(),
  CallState::tuple()
) -> {
    ok | transmit, Action::atom(), State::tuple()
  } | {
    deny, Sign::atom(), State::tuple()
  }.
before_dispatch(Controller, Action, State) ->
  case Controller:dispatch_filter(Action, State) of
    {ok, Action, NewState} ->
      {ok, Action, NewState};
    {transmit, Action, NewState} ->
      {transmit, Action, NewState};
    {deny, Sign, NewState} ->
      {deny, Sign, NewState};
    _Other ->
      io:format("Action in behaviour: ~p~n", [Action]),
      throw({dispatch_filter, error})
  end.

do_ack(AckCode, Params) ->
  Acknowledger = gg_config:get_acknowledger(),
  Acknowledger:acknowledge({AckCode, Params}).
