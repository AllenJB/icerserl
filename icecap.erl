%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(icecap).
-compile(export_all).
-import(lists, [reverse/1]).


start_server() ->
	ServerPid = self(),
	ManagerPid = spawn(fun() -> client_manager:client_manager([]) end),
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, line},
		{reuseaddr, true},
		{active, true}]),
	spawn(fun() -> connect_client(ManagerPid, ServerPid, Listen) end),
	receive
		cmdShutdown ->
			io:format("start_server: Shutting down listen socket~n"),
			gen_tcp:close(Listen),
			io:format("start_server: Notifying client manager~n"),
			ManagerPid ! {server_shutdown}
	end.


connect_client(ManagerPid, ServerPid, Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			io:format("Client connection accepted~n"),
			spawn(fun() -> connect_client(ManagerPid, ServerPid, Listen) end),
			%% Send the preauth notice
			ManagerPid ! {add, self()},
			%ManagerPid ! {broadcast, "*;client_connected;id=NotImplemented;time=NotImplemented", Socket},
			event_preauth(Socket),
			loop(Socket, ServerPid, ManagerPid);
		{error, closed} ->
			io:format("Listen socket closed~n")
	end.


event_preauth(Socket) ->
	{ok, {SockIP, _}} = inet:peername(Socket),
	IPStr = inet_parse:ntoa(SockIP),
	{MegaSecs, Secs, MicroSecs} = erlang:now(),
	MicroS = MicroSecs div 10000,
	SendStr0 = io_lib:format("*;preauth;time=~p~p.~p;remote_ip=", [MegaSecs, Secs, MicroS]),
	SendStr = string:concat(SendStr0, string:concat(IPStr, "\n")),
	gen_tcp:send(Socket, SendStr).


loop(Socket, ServerPid, ManagerPid) ->
	receive
		{tcp, Socket, <<"quit\r\n">>} ->
			io:format("Client closed socket~n"),
			ManagerPid ! {remove, self()},
			gen_tcp:close(Socket);
		{tcp, Socket, <<"sd\r\n">>} ->
			io:format("Client requested server shutdown~n"),
			ManagerPid ! {remove, self()},
			ServerPid ! cmdShutdown;
		{tcp, Socket, <<"b\r\n">>} ->
			io:format("Broacast Message~n"),
			ManagerPid ! {broadcast, "This is a broadcast message.~n", self()},
			loop(Socket, ServerPid, ManagerPid);
		{tcp, Socket, <<"p\r\n">>} ->
			io:format("Print Connected Clients~n"),
			ManagerPid ! {print},
			loop(Socket, ServerPid, ManagerPid);
		{tcp, Socket, Bin} ->
			io:format("Server received binary = ~p~n", [Bin]),
			io:format("Server replying = ~p~n", [Bin]),
			gen_tcp:send(Socket, Bin),
			loop(Socket, ServerPid, ManagerPid);
		{tcp_closed, Socket} ->
			io:format("Server socket closed~n"),
			ManagerPid ! {remove, self()};
		{broadcast, Msg, SrcClient} ->
			io:format("Sending broadcast message to client (0)~n", []),
			MyPid = self(),
			case SrcClient of
				MyPid ->
					io:format("Ignoring message from this client~n", []);
				Anything ->
					io:format("Sending broadcast message to client~n", []),
					gen_tcp:send(Socket, io_lib:format (string:concat("broadcast: ", Msg), []))
			end,
			loop(Socket, ServerPid, ManagerPid);
		Anything ->
			io:format("Unhandled: ~p~n", [Anything]),
			loop(Socket, ServerPid, ManagerPid)
	end.
