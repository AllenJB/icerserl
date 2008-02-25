-module(client_manager).
-compile(export_all).

client_manager() ->
	receive
		{add, Client} ->
			io:format("client_manager: Adding client: ~p~n", Client),
			case get(pidList) of
				undefined ->
					put (pidList, [Client]);
				[] ->
					put (pidList, [Client]);
				[L] ->
					put (pidList, [Client|L])
			end;
		{remove, Client} ->
			io:format("client_manager: Removing client: ~p~n", Client),
			put (pidList, lists:delete(Client, get(pidList)));
		{broadcast, Msg, SrcClient} ->
			io:format("client_manager: Broadcast from ~p: ~p~n", Msg, SrcClient),
			lists:map (fun(Client) -> Client ! {broadcast, Msg, SrcClient} end, get(pidList));
		{print} ->
			lists:map (fun(Client) -> io:format("Connected client: ~p~n", Client) end);
		Anything ->
			io:format("anything: ~p~n", [Anything])
	end,
	client_manager().
