%% @doc Chat server implementation
-module(chatServer).
-export([start/0, stop/0, find/0]).

-spec start() -> pid().
%% @doc start server and register it as 'chatServer'
start() ->
	CurPid = whereis(chatServer),
	case CurPid of
		undefined ->
			start_internal();
		_ ->
			io:format('Server already started.~n')
	end.

start_internal() ->
	Pid = spawn(fun loop/0),
	register(chatServer, Pid),
	io:format('Server is started.~n'),
	chatStorage:start(),
	Pid.

-spec stop() -> any().
%% @doc stop current server, if it is started
stop() ->
	Pid = whereis(chatServer),
	case Pid of
		undefined ->
			io:format('Server isn\'t started.~n');
		_ ->
			Pid ! {self(), exit},
			chatStorage:stop()
	end.

-spec find() -> pid().
%% @doc find current chat server pid, if it is started
find() ->
	Pid = whereis(chatServer),
	Pid.

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
			From ! {self(), {names, cur_time(), Names}},
			loop();
		{From, {message, Message}} ->
			client_message(From, Message),
			loop();
		{_, exit} ->
			unregister(chatServer),
			io:format('Server is stopped.~n'),
			exit(normal);
		Other ->
			io:format('Unknown signal: ~p.~n', [Other]),
			exit(error)
	end.

client_connect(From, Name) ->
	io:format('New client: ~s.~n', [Name]),
	Connect = put(Name, From),
	case Connect of
		undefined ->
			From ! {self(), welcome, cur_time()},
			notify_names(),
			send_history(From);
		_ ->
			io:format('Already logged.~n'),
			From ! {self(), exit}
	end.

send_history(To) ->
	History = chatStorage:history(),
	io:format('History: ~p.~n', [History]),
	lists:map(fun(Item) ->
		[{_, Message}] = Item,
		To ! {self(), Message}
	end, History).

client_message(FromPid, Message) ->
	FromName = client_name(FromPid),
	FullMessage = {message, cur_time(), FromName, Message},
	notify_clients(FullMessage),
	chatStorage:store(FullMessage).

client_disconnect(From) ->
	io:format('Client ~p leaved.~n', [From]),
	remove_client(From),
	From ! {self(), exit, cur_time()},
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
	notify_clients({names, cur_time(), Names}).

notify_clients(Message) ->
	Pids = client_pids(),
	lists:map(fun(Pid) -> Pid ! {self(), Message} end, Pids).

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

cur_time() ->
	calendar:local_time().

