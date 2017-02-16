-module(chatServer).
-export([start/0, stop/0]).

start() ->
	Pid = spawn(fun loop/0),
	register(chatServer, Pid),
	io:format('Server is started.~n'),
	Pid.

stop() ->
	Pid = whereis(chatServer),
	case Pid of
		undefined ->
			io:format('Server isn\'t started.~n');
		_ ->
			Pid ! {self(), exit}
	end.

loop() ->
	receive
		{From, {login, Name}} ->
			client_connect(From, Name),
			loop();
		{From, leave} ->
			client_disconnect(From),
			loop();
		{From, who} ->
			Names = client_names(),
			From ! {self(), {names, Names}},
			loop();
		{From, {message, Message}} ->
			client_message(From, Message),
			loop();
		{_, exit} ->
			unregister(chatServer),
			io:format('Server is stopped.~n'),
			exit(normal);
		_ -> 
			exit(error)
	end.

client_connect(From, Name) ->
	io:format('New client: ~s.~n', [Name]),
	Connect = put(Name, From),
	case Connect of
		undefined ->
			From ! {self(), welcome},
			notify_names();
		_ ->
			io:format('Already logged.~n'),
			From ! {self(), exit}
	end.

client_message(FromPid, Message) ->
	FromName = client_name(FromPid),
	notify_clients({message, FromName, Message}).

client_disconnect(From) ->
	io:format('Client ~p leaved.~n', [From]),
	remove_client(From),
	From ! {self(), exit},
	notify_names().

remove_client(From) ->
	Name = client_name(From),
	case Name of
		undefined ->
			io:format('Client not found.~n');
		_ ->
			erase(Name)
	end.

notify_names() ->
	Names = client_names(),
	notify_clients({names, Names}).

notify_clients(Message) ->
	Pids = client_pids(),
	notify_clients(Pids, Message).

notify_clients([H|T], Message) ->
	H ! {self(), Message},
	notify_clients(T, Message);

notify_clients([], _) ->
	true.

client_name(Pid) ->
	Keys = get(),
	client_name(Pid, Keys).

client_name(Pid, [H|T]) ->
	{Name, CurPid} = H,
	case CurPid == Pid of
		true ->
			Name;
		false ->
			client_name(Pid, T)
	end;

client_name(_, []) ->
	undefined.

client_names() ->
	Keys = get(),
	Names = client_names([], Keys),
	Names.

client_names(Names, [H|T]) ->
	{Name, _} = H,
	NewNames = [Name|Names],
	client_names(NewNames, T);

client_names(Names, []) ->
	lists:reverse(Names).

client_pids() ->
	Keys = get(),
	Pids = client_pids([], Keys),
	Pids.

client_pids(Pids, [H|T]) ->
	{_, Pid} = H,
	NewPids = [Pid|Pids],
	client_pids(NewPids, T);

client_pids(Pids, []) ->
	Pids.

