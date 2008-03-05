-module(event_manager).
-compile(export_all).


%% Processes events, distributing them to the correct processes
%% In most cases they'll be broadcast
%%   but in certain cases they'll only be returned to the source client
event_manager(ClientMgr, Events, Id) ->
	receive
		{add, Source, Destination, Event} ->
			case Destination of
				all ->
					ClientMgr ! {broadcast, {event, Id, Source, Event}, Source};
				source ->
					Source ! {event, Id, Event}
			end,
			event_manager(ClientMgr, Events ++ Event, Id + 1);
		{shutdown} ->
			void
	end.
