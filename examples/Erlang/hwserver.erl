%% "Hello world" server example.
-module(hwserver).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	{ok,Socket} = erlzmq:socket(Context,rep),
	ok = erlzmq:bind(Socket,"tcp://*:5555"),

	loop(Socket).

loop(Socket) ->
	% Wait for request
	{ok,Msg} = erlzmq:recv(Socket),
	io:format("Received request: ~s~n",[Msg]),

	% Some work
	timer:sleep(1000),

	% Send reply
	ok = erlzmq:send(Socket,<<"World">>),
	loop(Socket).

