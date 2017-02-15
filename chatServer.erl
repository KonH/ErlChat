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
			connectClient(From, Name),
			loop();
		{From, leave} ->
			clientLeaved(From),
			loop();
		{From, exit} ->
			unregister(chatServer),
			io:format('Server is stopped.~n'),
			From ! {self(), exit},
			exit(normal);
		_ -> 
			exit(error)
	end.

connectClient(From, Name) ->
	io:format('New client: ~s.~n', [Name]),
	Connect = put(Name, From),
	case Connect of
		undefined ->
			From ! {self(), welcome},
			Names = collectNames(),
			From ! {self(), names, Names};
		_ ->
			io:format('Already logged.~n'),
			From ! {self(), exit}
	end.

collectNames() ->
	Keys = get(),
	Names = collectNames([], Keys),
	Names.

collectNames(Names, [H|T]) ->
	{Name, _} = H,
	NewNames = [Name|Names],
	collectNames(NewNames, T);

collectNames(Names, []) ->
	lists:reverse(Names).

clientLeaved(From) ->
	From ! {self(), exit}.
