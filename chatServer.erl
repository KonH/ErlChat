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
		{From, login, Name} ->
			client_connect(From, Name),
			loop();
		{From, leave} ->
			client_disconnect(From),
			loop();
		{From, exit} ->
			unregister(chatServer),
			io:format('Server is stopped.~n'),
			From ! {self(), exit},
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
			Names = client_names(),
			From ! {self(), names, Names};
		_ ->
			io:format('Already logged.~n'),
			From ! {self(), exit}
	end.

client_disconnect(From) ->
	io:format('Client ~p leaved.~n', [From]),
	From ! {self(), exit}.

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

