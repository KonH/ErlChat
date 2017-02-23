%% @doc Chat message storage implementation
-module(chatStorage).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, store/1, history/0, stop/0]).

% helper methods

-spec start() -> {ok, pid()}.
% @doc start chat message storage
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> void.
% @doc stop current chat message storage
stop() ->
	gen_server:call(?MODULE, {stop}).

-spec store(Message::any()) -> boolean().
% @doc store message to chat message storage
store(Message) ->
	gen_server:call(?MODULE, {store, Message}).

-spec history() -> [].
% @doc get current messages history
history() ->
	gen_server:call(?MODULE, {history}).

% gen_server callbacks

init([]) ->
	open(history, "history.dets").

handle_call({store, Message}, _From, State) ->
	Reply = add_message(Message, State),
	{reply, Reply, State};

handle_call({history}, _From, State) ->
	Reply = history(State),
	{reply, Reply, State};

handle_call({stop}, _From, State) ->
	Reply = close(State),
	{stop, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	close(State).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% init handler

open(Table, File) ->
	io:format("Open/create file: ~p.~n" , [File]),
	Exist = filelib:is_file(File),
	Result = dets:open_file(Table, [{file, File}]),
	case Result of
		{ok, Table} ->
			case Exist of
				true -> void;
				false -> ok = dets:insert(Table, {count, 1})
			end;
		{error, _Reason} ->
			io:format("Can't open file.~n" )
	end,
	Result.

% store handler

add_message(Message, Table) ->
	[{_, MessageId}] = dets:lookup(Table, count),
	dets:insert(Table, {MessageId, Message}),
	update_count(Table),
	true.

update_count(Table) ->
	[{_, Count}] = dets:lookup(Table, count),
	NewCount = Count + 1,
	dets:insert(Table, {count, NewCount}).

% history handler

history(Table) ->
	SkipKey = dets:first(Table),
	Key = dets:next(Table, SkipKey),
	History = history(Table, Key, []),
	History.

history(Table, Key, Acc) ->
	case Key of
		'$end_of_table' ->
			Acc;
		_ ->
			Value = dets:lookup(Table, Key),
			NextKey = dets:next(Table, Key),
			history(Table, NextKey, [Value|Acc])
	end.

% terminate handler

close(Table) ->
	dets:close(Table).
