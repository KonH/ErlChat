-module(chatStorage).
-export([start/0, store/1, history/1, stop/0]).

-spec start() -> pid().
%% @doc start chat storage and register it as 'chatStorage'
start() ->
	CurPid = whereis(chatStorage),
	case CurPid of
		undefined ->
			start_internal();
		_ ->
			io:format('Storage already started.~n')
	end.

start_internal() ->
	Pid = spawn(fun loop/0),
	register(chatStorage, Pid),
	Pid ! {self(), start},
	io:format('Storage is started.~n'),
	Pid.

-spec store(Message::any()) -> any().
%% @doc store message to current chat storage
store(Message) ->
	Pid = whereis(chatStorage),
	case Pid of
		undefined ->
			start_internal(),
			store(Message);
		_ ->
			Pid ! {self(), store, Message}
	end.

add_message(Message) ->
	[{_, MessageId}] = dets:lookup(history, count),
	dets:insert(history, {MessageId, Message}).

update_count() ->
	[{_, Count}] = dets:lookup(history, count),
	NewCount = Count + 1,
	dets:insert(history, {count, NewCount}).

-spec stop() -> any().
%% @doc stop current chat storage if it is started
stop() ->
	Pid = whereis(chatStorage),
	case Pid of
		undefined ->
			io:format('Storage isn\'t started.~n');
		_ ->
			Pid ! {self(), exit}
	end.

loop() ->
	receive
		{_, start} ->
			open(),
			loop();
		{_, store, Message} ->
			add_message(Message),
			update_count(),
			loop();
		{From, {history, To}} ->
			History = self_history(),
			From ! {self(), {history, To, History}},
			loop();
		{_, exit} ->
			unregister(chatStorage),
			close(),
			io:format('Storage is stopped.~n'),
			exit(normal);
		Signal ->
			io:format('Unknown signal: ~p.~n', [Signal])
	end.

open() ->
	File = "history.dets",
	io:format("Open/create file: ~p.~n" , [File]),
	Bool = filelib:is_file(File),
	case dets:open_file(history, [{file, File}]) of
		{ok, history} ->
			case Bool of
				true -> void;
				false -> ok = dets:insert(history, {count, 1})
			end,
			true;
		{error,_Reason} ->
			io:format("Can't open file.~n" ),
			exit(error)
	end.

close() ->
	dets:close(history).

-spec history(ClientPid::pid()) -> any().
%% @doc retrieve current chat history and send it to provided pid
history(ClientPid) ->
	Pid = whereis(chatStorage),
	Pid ! {self(), {history, ClientPid}}.

self_history() ->
	SkipKey = dets:first(history),
	Key = dets:next(history, SkipKey),
	History = self_history(Key, []),
	History.

self_history(Key, Acc) ->
	case Key of
		'$end_of_table' ->
			RevAcc = lists:reverse(Acc),
			RevAcc;
		_ ->
			Value = dets:lookup(history, Key),
			NextKey = dets:next(history, Key),
			self_history(NextKey, [Value|Acc])
	end.
