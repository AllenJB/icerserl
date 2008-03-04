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
			event_preauth(ManagerPid, Socket),
			loop(Socket, ServerPid, ManagerPid);
		{error, closed} ->
			io:format("Listen socket closed~n")
	end.


event_preauth(ManagerPid, Socket) ->
	{ok, {SockIP, _}} = inet:peername(Socket),
	IPStr = inet_parse:ntoa(SockIP),
	{MegaSecs, Secs, MicroSecs} = erlang:now(),
	MicroS = MicroSecs div 10000,
	SendStr0 = io_lib:format("*;preauth;time=~p~p.~p;remote_ip=", [MegaSecs, Secs, MicroS]),
	SendStr = string:concat(SendStr0, string:concat(IPStr, "\n")),
	gen_tcp:send(Socket, SendStr),
	SendStr1 = io_lib:format("*;client_connected;id=NotImplemented;time=~p~p.~p~n", [MegaSecs, Secs, MicroS]),
	ManagerPid ! {broadcast, SendStr1, self()}.


loop(Socket, ServerPid, ManagerPid) ->
	receive
		{tcp, Socket, Msg} ->
			process_line (ServerPid, ManagerPid, Socket, Msg),
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
				_ ->
					io:format("Sending broadcast message to client~n", []),
					gen_tcp:send(Socket, io_lib:format (string:concat("broadcast: ", Msg), []))
			end,
			loop(Socket, ServerPid, ManagerPid);
		{quit} ->
			void;
		Anything ->
			io:format("Unhandled: ~p~n", [Anything]),
			loop(Socket, ServerPid, ManagerPid)
	end.


process_line(ServerPid, ManagerPid, Socket, BitStringMsgIn) ->
	MsgIn = bitstring_to_list(BitStringMsgIn),
	Msg = string:strip(string:strip(MsgIn, right, 10), right, 13),
	Command = parse_command(Msg),
	case lists:keysearch("command", 1, Command) of
		{value, {"command", "quit"}} ->
			io:format("Client closed socket~n"),
			ManagerPid ! {remove, self()},
			gen_tcp:close(Socket),
			self() ! {quit};
		{value, {"command", "sd"}} ->
			io:format("Client requested server shutdown~n"),
			ManagerPid ! {remove, self()},
			ServerPid ! cmdShutdown;
		{value, {"command", "b"}} ->
			io:format("Broacast Message~n"),
			ManagerPid ! {broadcast, "This is a broadcast message.~n", self()};
		{value, {"command", "p"}} ->
			io:format("Print Connected Clients~n"),
			ManagerPid ! {print};
		false ->
			io:format("No command given: ~p: ~p~n", [self(), Msg]),
			gen_tcp:send(Socket, io_lib:format("No command given: ~p~n", [Msg]));
		Anything ->
			io:format("Unrecognised command: ~p: ~p~n", [self(), Msg]),
			gen_tcp:send(Socket, io_lib:format("Unrecognised command: ~p~n", [Anything]))
	end.


%% Parse a command string and return the hashmap of key/value pairs
%% Format: key=value;key1=value1
parse_command(Command) ->
	Pairs = string:tokens(Command, ";"),
	parse_command_pair(Pairs, []).

parse_command_pair([], Hash) ->
	Hash;
parse_command_pair([""|Pairs], Hash) ->
	parse_command_pair(Pairs, Hash);
parse_command_pair([H|Pairs], Hash) ->
	KVals = string:tokens(H, "="),
	if
		length(KVals) == 0 ->
			parse_command_pair(Pairs, Hash);
		true ->
			void
	end,
	Value = case length(KVals) of
		1 ->
			undefined;
		_ ->
			string:join(lists:nthtail(1, KVals), "=")
	end,
	NewHash = Hash ++ [{hd(KVals), Value}],
	parse_command_pair(Pairs, NewHash).
