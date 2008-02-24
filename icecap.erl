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
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, line},
		{reuseaddr, true},
		{active, true}]),
	spawn(fun() -> connect_client(ServerPid, Listen) end),
	receive
		cmdShutdown ->
			io:format("start_server: Shutting down listen socket~n"),
			gen_tcp:close(Listen)
	end.


connect_client(ServerPid, Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			io:format("Client connection accepted"),
			spawn(fun() -> connect_client(ServerPid, Listen) end),
			%% Send the preauth notice
			event_preauth(Socket),
			loop(Socket, ServerPid);
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


loop(Socket, ServerPid) ->
	receive
		{tcp, Socket, <<"quit\r\n">>} ->
			io:format("Client closed socket~n"),
			gen_tcp:close(Socket);
		{tcp, Socket, <<"sd\r\n">>} ->
			io:format("Client requested server shutdown~n"),
			ServerPid ! cmdShutdown;
		{tcp, Socket, Bin} ->
			io:format("Server received binary = ~p~n",[Bin]),
			io:format("Server replying = ~p~n",[Bin]),
			gen_tcp:send(Socket, Bin),
			%% when you're ready enable the next message
			% inet:setopts(Socket, [{active, once}]),
			loop(Socket, ServerPid);
		{tcp_closed, Socket} ->
			 io:format("Server socket closed~n")
	end.
