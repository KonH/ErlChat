-module(chatClient).
-export([connect/1, disconnect/1, who_is_here/1, message/2]).

connect(Name) ->
	ServerPid = whereis(chatServer),
	connect(Name, ServerPid).

connect(_, undefined) ->
	io:format('Can\'t find server.~n'),
	undefined;

connect(Name, ServerPid) ->
	io:format('Connect to ~p.~n', [ServerPid]),
	ClientPid = spawn(fun loop/0),
	ServerPid ! {ClientPid, {login, Name}},
	ClientPid.

who_is_here(Pid) ->
	server_ask(Pid, who).

message(Pid, Message) ->
	server_ask(Pid, {message, Message}).

disconnect(Pid) ->
	server_ask(Pid, leave).

loop() ->
	receive
		{_, welcome} ->
			io:format('~p: You are welcome!~n', [self()]),
			loop();
		{_, {names, Names}} ->
			io:format('~p: Clients here: ~p.~n', [self(), Names]),
			loop();
		{_, {message, From, Message}} ->
			io:format('~p: ~p: ~p~n', [self(), From, Message]),
			loop();
		{_, exit} ->
			io:format('~p: Client disconnected.~n', [self()]),
			exit(normal);
		_ ->
			exit(error)
	end.

server_ask(Pid, What) ->
	ServerPid = whereis(chatServer),
	ServerPid ! {Pid, What}.
