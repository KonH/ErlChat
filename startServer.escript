#!/usr/bin/env escript
%%! -sname chatServer

main(_) ->
	chatServer:start(),
	Node = node(),
	Pid = chatServer:find(),
	io:format('Node: ~p, pid: ~p.~n', [Node, Pid]),
	loop().

loop() ->
	loop().
