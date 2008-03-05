-module(icecap).
-compile(export_all).


start_server() ->
	ServerPid = self(),
	ManagerPid = spawn(fun() -> client_manager:client_manager([]) end),
	EventMgr = spawn(fun() -> event_manager:event_manager(ManagerPid, [], 0) end),
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, line},
		{reuseaddr, true},
		{active, true}]),
	spawn(fun() -> connect_client(EventMgr, ManagerPid, ServerPid, Listen) end),
	receive
		cmdShutdown ->
			gen_tcp:close(Listen),
			ManagerPid ! {server_shutdown},
			EventMgr ! {server_shutdown}
	end.


connect_client(EventMgr, ManagerPid, ServerPid, Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			io:format("Client connection accepted~n"),
			spawn(fun() -> connect_client(EventMgr, ManagerPid, ServerPid, Listen) end),
			ManagerPid ! {add, self()},
			event_preauth(EventMgr, Socket),
			loop(Socket, ServerPid, ManagerPid);
		{error, closed} ->
			io:format("Listen socket closed~n")
	end.


event_preauth(EventMgr, Socket) ->
	{ok, {SockIP, _}} = inet:peername(Socket),
	EventMgr ! {add, self(), source, {preauth, now(), [{remote_ip, inet_parse:ntoa(SockIP)}]}}.


get_timestamp({MegaSecs, Secs, MicroSecs}) ->
	integer_to_list(MegaSecs) ++ integer_to_list(Secs) ++ "." ++ integer_to_list(MicroSecs).


loop(Socket, ServerPid, ManagerPid) ->
	receive
		{tcp, Socket, Msg} ->
			process_line (ServerPid, ManagerPid, Socket, Msg),
			loop(Socket, ServerPid, ManagerPid);
		{tcp_closed, Socket} ->
			io:format("Server socket closed~n"),
			ManagerPid ! {remove, self()};
		{broadcast, Event, SrcClient} ->
			MyPid = self(),
			case SrcClient of
				MyPid ->
					void;
				_ ->
					gen_tcp:send(Socket, event_to_string(Event))
			end,
			loop(Socket, ServerPid, ManagerPid);
		{event, Id, Event} ->
			gen_tcp:send(Socket, event_to_string({event, Id, Event})),
			loop(Socket, ServerPid, ManagerPid);
		{server_shutdown} ->
			gen_tcp:send(Socket, "*;server_shutdown\n"),
			gen_tcp:close(Socket);
		{quit} ->
			void;
		Anything ->
			io:format("Unhandled: ~p~n", [Anything]),
			loop(Socket, ServerPid, ManagerPid)
	end.


event_to_string({event, Id, {EventType, TimeStamp, Params}}) ->
	io_lib:format("*;"++ atom_to_list(EventType) ++";id=~p;time="++ get_timestamp(TimeStamp) ++";"++ params_to_string(Params) ++"~n", [Id]).


params_to_string([]) ->
	"";
params_to_string(Params) ->
	string:join(kvpairs_to_string(Params, []), ";").


kvpairs_to_string([], List) ->
	List;
kvpairs_to_string([{Key, Value}|Params], List) ->
	kvpairs_to_string(Params, List ++ [atom_to_list(Key) ++ "=" ++ Value]).


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
