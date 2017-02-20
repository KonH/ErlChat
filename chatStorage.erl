-module(chatStorage).
-export([start/0, store/1, history/1, stop/0]).

-spec start() -> pid().
%% @doc start chat storage and register it as 'chatStorage'
start() ->
	Pid = spawn(fun loop/0),
	register(chatStorage, Pid),
	Pid ! {self(), start},
	io:format('Storage is started.~n'),
	Pid.

-spec store(Message::any()) -> any().
%% @doc store message to current chat storage
store(Message) ->
	Pid = whereis(chatStorage),
	Pid ! {self(), store, Message}.

add_message(Message) ->
	TableId = get(history),
	MessageId = get(count), 
	ets:insert(TableId, {MessageId, Message}).

update_count() ->
	Count = get(count),
	NewCount = Count + 1,
	put(count, NewCount).

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
	TableId = ets:new(history, [ordered_set]),
	put(count, 0),
	put(history, TableId),
	update_count().

close() ->
	TableId = get(history),
	ets:delete(TableId).

-spec history(ClientPid::pid()) -> any().
%% @doc retrieve current chat history and send it to provided pid
history(ClientPid) ->
	Pid = whereis(chatStorage),
	Pid ! {self(), {history, ClientPid}}.

self_history() ->
	TableId = get(history),
	self_history(TableId).

self_history(TableId) ->
	Key = ets:first(TableId),
	History = self_history(TableId, Key, []),
	History.

self_history(TableId, Key, Acc) ->
	case Key of
		'$end_of_table' ->
			RevAcc = lists:reverse(Acc),
			RevAcc;
		_ ->
			Value = ets:lookup(TableId, Key),
			NextKey = ets:next(TableId, Key),
			self_history(TableId, NextKey, [Value|Acc])
	end.
