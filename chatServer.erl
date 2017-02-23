%% @doc Chat server implementation
-module(chatServer).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, stop/0, login/1, leave/0, who/0, message/1]).

% helper methods

-spec start() -> {ok, pid()}.
%% @doc start chat server
start() ->
	chatStorage:start(),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> void.
%% @doc stop current server, if it is started
stop() ->
	chatStorage:stop(),
	gen_server:call(?MODULE, {stop}).

-spec login(Name::atom()) -> void.
%% @doc login to server
login(Name) ->
	gen_server:call(?MODULE, {login, Name}).

-spec leave() -> void.
%% @doc leave server
leave() ->
	gen_server:call(?MODULE, {leave}).

-spec who() -> [].
%% @doc get current clients
who() ->
	gen_server:call(?MODULE, {who}).

-spec message(Message::any()) -> void.
%% @doc send message
message(Message) ->
	gen_server:call(?MODULE, {message, Message}).

% gen_server callbacks

init([]) ->
	State = init(),
	{ok, State}.

handle_call({login, Name}, From, State) ->
	State1 = add_client(From, Name, State),
	Reply = login(From, Name, State1),
	{reply, Reply, State1};

handle_call({leave}, From, State) ->
	State1 = leave_state(From, State),
	Reply = leave_reply(From, State1),
	{reply, Reply, State1};

handle_call({who}, From, State) ->
	Reply = who(From, State),
	{reply, Reply, State};

handle_call({message, Message}, From, State) ->
	Reply = message(From, Message, State),
	{reply, Reply, State};

handle_call({stop}, _From, State) ->
	{stop, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	terminate(),
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% init handler

init() -> 
	io:format('Start chat server.~n'),
	[].

% login handler
login(From, Name, State) ->
	io:format('New client: ~p.~n', [Name]),
	{FromPid, _Tag} = From,
	FromPid ! {self(), welcome, cur_time()},
	notify_names(State),
	send_history(From),
	true.

send_history(To) ->
	History = chatStorage:history(),
	lists:map(fun(Item) ->
		[{_, Message}] = Item,
		{ToPid, _tag} = To,
		ToPid ! {self(), Message}
	end, History).

add_client(From, Name, State) ->
	NewState = [{Name, From}|State],
	NewState.

% leave handler
leave_state(From, State) ->
	io:format('Client ~p leaved.~n', [From]),
	NewState = remove_client(From, State),
	NewState.

leave_reply(From, NewState) ->
	{FromPid, _Tag} = From,
	FromPid ! {self(), exit, cur_time()},
	notify_names(NewState).

remove_client(From, State) ->
	Name = client_name(From, State),
	NewState = remove_client([], State, Name),
	NewState.

remove_client(State, [], _Name) ->
	State;

remove_client(State, [H|T], Name) ->
	{CurName, _} = H,
	case CurName of
		Name ->
			remove_client(State, T, Name);
		_ ->
			remove_client([H|State], T, Name)
	end.

% who handler

who(From, State) ->
	Names = client_names(State),
	{FromPid, _Tag} = From,
	FromPid ! {self(), {names, cur_time(), Names}},
	ok.

% message handler

message(FromPid, Message, State) ->
	FromName = client_name(FromPid, State),
	FullMessage = {message, cur_time(), FromName, Message},
	notify_clients(FullMessage, State),
	chatStorage:store(FullMessage),
	ok.

% terminate handler

terminate() ->
	io:format('Stop chat server.~n').

% notifies

notify_names(State) ->
	Names = client_names(State),
	notify_clients({names, cur_time(), Names}, State).

notify_clients(Message, State) ->
	Pids = client_pids(State),
	lists:map(fun(Item) -> {Pid, _Tag} = Item, Pid ! {self(), Message} end, Pids).

% utility

client_pids(State) ->
	Pids = client_pids([], State),
	Pids.

client_pids(Pids, [H|T]) ->
	{_, Pid} = H,
	NewPids = [Pid|Pids],
	client_pids(NewPids, T);

client_pids(Pids, []) ->
	Pids.

client_names(State) ->
	Names = client_names([], State),
	Names.

client_names(Names, [H|T]) ->
	{Name, _} = H,
	NewNames = [Name|Names],
	client_names(NewNames, T);

client_names(Names, []) ->
	lists:reverse(Names).

client_name(PidItem, [H|T]) ->
	{Name, CurPidItem} = H,
	{CurPid, _} = CurPidItem,
	{Pid, _} = PidItem,
	case CurPid == Pid of
		true ->
			Name;
		false ->
			client_name(Pid, T)
	end;

client_name(_, []) ->
	undefined.

cur_time() ->
	calendar:local_time().
