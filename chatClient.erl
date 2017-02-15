-module(chatClient).
-export([connect/1, disconnect/1]).

connect(Name) ->
	ServerPid = whereis(chatServer),
	connect(Name, ServerPid).

connect(_, undefined) ->
	io:format('Can\'t find server.~n'),
	undefined;

connect(Name, ServerPid) ->
	io:format('Connect to ~p.~n', [ServerPid]),
	ClientPid = spawn(fun loop/0),
	ServerPid ! {ClientPid, login, Name},
	ClientPid.

disconnect(Pid) ->
	ServerPid = whereis(chatServer),
	ServerPid ! {Pid, leave}.

loop() ->
	receive
		{_, welcome} ->
			io:format('You are welcome!~n'),
			loop();
		{_, names, Names} ->
			io:format('Clients here: ~p.~n', [Names]),
			loop();
		{_, exit} ->
			io:format('Client disconnected.~n'),
			exit(normal);
		_ ->
			exit(error)
	end.
