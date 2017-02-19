-module(chatStorage).
-export([start/0, store/1, stop/0]).

start() ->
	Pid = spawn(fun loop/0),
	register(chatStorage, Pid),
	Pid ! {self(), start},
	io:format('Storage is started.~n'),
	Pid.

store(Message) ->
	Pid = whereis(chatStorage),
	Pid ! {self(), store, Message}.

add_message(Message) ->
	TableId = get(history),
	MessageId = get(count), 
	ets:insert(TableId, {MessageId, Message}),
	io:format('Message ~p saved to ~p.~n', [Message, MessageId]),
	Value = ets:lookup(TableId, MessageId),
	io:format('Message at ~p is ~p.~n', [MessageId, Value]).

update_count() ->
	Count = get(count),
	NewCount = Count + 1,
	put(count, NewCount),
	io:format('New count is ~p.~n', [NewCount]).

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
		{_, exit} ->
			unregister(chatStorage),
			close(),
			io:format('Storage is stopped.~n'),
			exit(normal);
		Signal ->
			io:format('Unknown signal: ~p.~n', [Signal])
	end.

open() ->
	TableId = ets:new(history, [ordered_set]),
	put(count, 0),
	put(history, TableId),
	add_message({message, {{2017,2,19},{20,46,5}}, test, 'test'}),
	update_count().

close() ->
	TableId = get(history),
	ets:delete(TableId).
