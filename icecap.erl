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


receive_data(Socket, SoFar) ->
	receive
		{tcp,Socket,Bin} ->    %% (3)
			receive_data(Socket, [Bin|SoFar]);
		{tcp_closed,Socket} -> %% (4)
			list_to_binary(reverse(SoFar)) %% (5)
	end.



nano_client_eval(Str) ->
	{ok, Socket} =
		gen_tcp:connect("localhost", 2345,
			[binary, {packet, 4}]),
	ok = gen_tcp:send(Socket, term_to_binary(Str)),
	receive
		{tcp,Socket,Bin} ->
			io:format("Client received binary = ~p~n",[Bin]),
			Val = binary_to_term(Bin),
			io:format("Client result = ~p~n",[Val]) %%,
%%			gen_tcp:close(Socket)
	end,
	ok = gen_tcp:send(Socket, term_to_binary(Str)),
	receive
		{tcp,Socket,Bin} ->
			io:format("Client received binary = ~p~n",[Bin]),
			Val = binary_to_term(Bin),
			io:format("Client result = ~p~n",[Val]) %%,
%%			gen_tcp:close(Socket)
	end,
	ok = gen_tcp:send(Socket, term_to_binary(Str)),
	receive
		{tcp,Socket,Bin} ->
			io:format("Client received binary = ~p~n",[Bin]),
			Val = binary_to_term(Bin),
			io:format("Client result = ~p~n",[Val]),
			gen_tcp:close(Socket)
	end.


start_server() ->
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, line},  %% (6)
		{reuseaddr, true},
		{active, true}]),
	{ok, Socket} = gen_tcp:accept(Listen),  %% (7)
	gen_tcp:close(Listen),
	loop(Socket).


loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			io:format("Server received binary = ~p~n",[Bin]),
			io:format("Server replying = ~p~n",[Bin]),
			gen_tcp:send(Socket, Bin),  %% (11)
			%% when you're ready enable the next message
			% inet:setopts(Socket, [{active, once}]),
			loop(Socket);
		{tcp_closed, Socket} ->
			 io:format("Server socket closed~n")
	end.


error_test() ->
	spawn(fun() -> error_test_server() end),
	lib_misc:sleep(2000),
	{ok,Socket} = gen_tcp:connect("localhost",4321,[binary, {packet, 2}]),
	io:format("connected to:~p~n",[Socket]),
	gen_tcp:send(Socket, <<"123">>),
	receive
		Any ->
			io:format("Any=~p~n",[Any])
	end.

error_test_server() ->
	{ok, Listen} = gen_tcp:listen(4321, [binary,{packet,2}]),
	{ok, Socket} = gen_tcp:accept(Listen),
	error_test_server_loop(Socket).

error_test_server_loop(Socket) ->
	receive
		{tcp, Socket, Data} ->
			io:format("received:~p~n",[Data]),
			atom_to_list(Data),
			error_test_server_loop(Socket)
	end.
