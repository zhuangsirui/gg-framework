-module(gg_test_helper).

-export([
  start/1
]).

-include_lib("include/config/api_action.hrl").
-include_lib("include/config/ack_action.hrl").

start(ActionList) ->
  {ok, Socket} = gen_tcp:connect("localhost", acceptor_port(), [binary, {packet, 2}, {active, false}]),
  spawn(fun() -> receive_message(Socket) end),
  run(ActionList, Socket),
  ok.

run([], _Socket) ->
  ok;
run([H | T], Socket) ->
  {ActionCode, Params} = H,
  Template = fetch_params_template(route, ActionCode),
  Data = gg_acknowledger:pack(Template, Params, ActionCode, 0),
  gen_tcp:send(Socket, Data),
  run(T, Socket).

receive_message(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("Receive data: ~p~n", [Data]),
      decode_ack(Data),
      receive_message(Socket);
    {error, Reason} ->
      io:format("Receive error: ~p~n", [Reason])
  end.

decode_ack(<<CallCode:16, ErrorCode:16, Data/binary>>) ->
  [ParamsTemplate | []] = gg_acknowledger:get_template(CallCode),
  {params, TemplateList} = ParamsTemplate,
  {params, Params} = gg_analysor:unpack(TemplateList, ErrorCode, Data),
  io:format("Unpack ack data: ~p~n", [Params]),
  ok.

fetch_params_template(Type, ActionCode) ->
  case Type of
    route ->
      {api, ApiRoute} = route(),
      ActionList = [Action || [Code | Action] <- ApiRoute, ActionCode == Code],
      [Action | []] = ActionList,
      [_Controller, _Action, {params, Template}] = Action,
      Template;
    ack ->
      ok
  end.

acceptor_port() ->
  gg_config:get_acceptor_port().
