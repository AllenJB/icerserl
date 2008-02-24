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
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, line},
		{reuseaddr, true},
		{active, true}]),
	{ok, Socket} = gen_tcp:accept(Listen),
	gen_tcp:close(Listen),
	% Send the preauth notice
	event_preauth(Socket),
	loop(Socket).


event_preauth(Socket) ->
	{ok, {SockIP, _}} = inet:peername(Socket),
	IPStr = inet_parse:ntoa(SockIP),
	{MegaSecs, Secs, MicroSecs} = erlang:now(),
	MicroS = MicroSecs div 10000,
	gen_tcp:send(Socket, io_lib:format("*;preauth;time=~p~p.~p;remote_ip=~p~n", [MegaSecs, Secs, MicroS, IPStr])).


loop(Socket) ->
	receive
		{tcp, Socket, <<"quit\r\n">>} ->
			io:format("Client closed socket~n"),
			gen_tcp:close(Socket);
		{tcp, Socket, Bin} ->
			io:format("Server received binary = ~p~n",[Bin]),
			io:format("Server replying = ~p~n",[Bin]),
			gen_tcp:send(Socket, Bin),
			%% when you're ready enable the next message
			% inet:setopts(Socket, [{active, once}]),
			loop(Socket);
		{tcp_closed, Socket} ->
			 io:format("Server socket closed~n")
	end.
