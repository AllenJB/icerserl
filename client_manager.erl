-module(client_manager).
-compile(export_all).

client_manager(Clients) ->
	receive
		{add, Client} ->
			io:format("client_manager: Adding client: ~p~n", [Client]),
			case Clients of
				[] ->
					put (pidList, [Client]);
				[L] ->
					put (pidList, [Client|Clients])
			end;
		{remove, Client} ->
			io:format("client_manager: Removing client: ~p~n", [Client]),
			put (pidList, lists:delete(Client, Clients));
		{broadcast, Msg, SrcClient} ->
			io:format("client_manager: Broadcast from: ~p: ~p~n", [SrcClient, Msg]),
			lists:map (fun(Client) -> Client ! {broadcast, Msg, SrcClient} end, Clients);
		{print} ->
			lists:map (fun(Client) -> io:format("Connected client: ~p~n", [Client]) end, Clients);
		Anything ->
			io:format("Unhandled: ~p~n", [Anything])
	end,
	client_manager().
