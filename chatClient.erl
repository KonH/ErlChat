%% @doc Chat client implementation
-module(chatClient).
-export([connect/1, disconnect/1, who_is_here/1, message/2]).

-spec connect(Name::string()) -> pid().
%% @doc connect to local node with client name and return its Pid
connect(Name) ->
	ClientPid = spawn(fun () -> client_routine(Name) end),
	ClientPid.

-spec who_is_here(Pid::pid()) -> any().
%% @doc retrive current logged users for client with Pid
who_is_here(Pid) ->
	Pid ! {self(), {ask_who}}.

-spec message(Pid::pid(), Message::any()) -> any().
%% @doc send any message to all logged clients from client with Pid
message(Pid, Message) ->
	Pid ! {self(), {ask_message, Message}}.

-spec disconnect(Pid::pid()) -> any().
%% @doc disconnect from server for client with Pid
disconnect(Pid) ->
	Pid ! {self(), {ask_leave}}.

client_routine(Name) ->
	chatServer:login(Name),
	loop().

loop() ->
	receive
		{_, {ask_who}} ->
			chatServer:who(),
			loop();
		{_, {ask_message, Message}} ->
			chatServer:message(Message),
			loop();
		{_, {ask_leave}} ->
			chatServer:leave(),
			loop();
		{_, welcome, Time} ->
			io:format('~p: [~s] You are welcome!~n', [self(), simply_time(Time)]),
			loop();
		{_, {names, Time, Names}} ->
			io:format('~p: [~s] Clients here: ~p.~n', [self(), simply_time(Time), Names]),
			loop();
		{_, {message, Time, From, Message}} ->
			io:format('~p: [~s] ~p: ~p~n', [self(), simply_time(Time), From, Message]),
			loop();
		{_, exit, Time} ->
			io:format('~p: [~s] Client disconnected.~n', [self(), simply_time(Time)]),
			exit(normal);
		{_, exit} ->
			exit(normal);
		X ->
			io:format('Unknown message: ~p~n', [X]),
			exit(error)
	end.

simply_time(T) ->
	{_, {H, M, _}} = T,
	io_lib:format("~2..0b:~2..0b", [H, M]).

