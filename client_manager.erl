-module(client_manager).
-compile(export_all).

client_manager(Clients) ->
	receive
		{add, Client} ->
			client_add(Clients, Client);
		{remove, Client} ->
			client_remove(Clients, Client);
		{broadcast, Msg, SrcClient} ->
			lists:map (fun(Client) -> Client ! {broadcast, Msg, SrcClient} end, Clients),
			client_manager(Clients);
		{print} ->
			lists:map (fun(Client) -> io:format("Connected client: ~p~n", [Client]) end, Clients),
			client_manager(Clients);
		{server_shutdown} ->
			lists:map (fun(Client) -> Client ! {server_shutdown} end, Clients);
		Anything ->
			io:format("Unhandled: ~p~n", [Anything]),
			client_manager(Clients)
	end.


client_remove (Clients, Client) ->
	io:format("client_manager: Removing client: ~p~n", [Client]),
	NewClients = lists:delete(Client, Clients),
	case get(serverShutdown) of
		true ->
			if
				length(NewClients) /= 0 ->
					client_manager(NewClients);
				true ->
					void
			end;
		undefined ->
			client_manager(NewClients);
		false ->
			io:format("Shutting down client_manager~n", [])
	end.


client_add ([], Client) ->
	client_manager([Client]);
client_add (Clients, Client) ->
	client_manager(Clients ++ [Client]).
