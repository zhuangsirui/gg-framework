%% -------------------------------------------------------------------
%% @author Sirius Zhuang <siriusibunny@gmail.com>
%%   [http://github.com/siriuszhuang]
%% @version 0.0.1
%% @doc This is a module for accept port to connect server. Default
%% acceptor for gg_framework.
%% @end
%% -------------------------------------------------------------------
-module(gg_acceptor).

%% API
-export([
  start_link/1,
  start_acceptor/1
]).

%% -------------------------------------------------------------------
%% @doc Called by acceptor supervisor to start acceptor. Send a ack
%% back for success.
%% @end
%% -------------------------------------------------------------------
start_link(Parent) ->
  io:format("~n================~n"),
  io:format("Init listen port.~n"),
  {ok, Pid} = proc_lib:start_link(?MODULE, start_acceptor, [Parent]),
  io:format("Listen port Success.~n"),
  {ok, Pid}.

%% -------------------------------------------------------------------
%% @doc Start acceptor.
%% -------------------------------------------------------------------
start_acceptor(Parent) ->
  io:format("Start listen port.~n"),
  case listen_port() of
    {ok, ListenSocket} ->
      io:format("Listen port done.~n"),
      proc_lib:init_ack(Parent, {ok, self()}),
      acceptor_loop(ListenSocket);
    {ErrorType, Reason} ->
      throw({listen_error, {ErrorType, Reason}})
  end.

%% -------------------------------------------------------------------
%% @doc Listen port.
%% -------------------------------------------------------------------
listen_port() ->
  Options = [
    binary,
    {packet, 2},
    {reuseaddr, true},
    {backlog, 1024},
    {active, false}
  ],
  AcceptorPort = gg_config:get_acceptor_port(),
  gen_tcp:listen(AcceptorPort, Options).

%% -------------------------------------------------------------------
%% @doc Keep accept port. Forward connection socket to server.
%% -------------------------------------------------------------------
acceptor_loop(ListenSocket) ->
  io:format("Acceptor loop try.~n"),
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      handle_connection(Socket),
      acceptor_loop(ListenSocket);
    {ErrorType, Reason} ->
      throw({acceptor_error, {ErrorType, Reason}})
  end.

%% -------------------------------------------------------------------
%% @doc Catch the connection and forward it to server module.
%% -------------------------------------------------------------------
handle_connection(Socket) ->
  io:format("Handle connection.~n"),
  {ok, ServerPid} = gg_server_sup:start_child(),
  ok = gen_tcp:controlling_process(Socket, ServerPid),
  Server = gg_config:get_server(),
  Server:set_socket(ServerPid, Socket).
