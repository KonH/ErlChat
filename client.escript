#!/usr/bin/env escript
%%! -sname chatClient

main(Arg) ->
	[NodeNameStr, Name] = Arg,
	NodeName = list_to_atom(NodeNameStr),
	io:format('Connect to ~p as ~p.~n', [NodeName, Name]),
	Pid = chatClient:connect(NodeName, Name),
	register(currentClient, Pid),
	loop().

loop() ->
	Message = io:get_line(""),
	Pid = whereis(currentClient),
	chatClient:message(Pid, Message),
	loop().
