%% "Hello world" client example.
-module(hwclient).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	io:format("Connecting to hello world server...~n",[]),
	{ok,Socket} = erlzmq:socket(Context,req),
	ok = erlzmq:connect(Socket,"tcp://localhost:5555"),

	loop(Socket,1),
	ok = erlzmq:close(Socket),
	ok = erlzmq:term(Context).

% Do 10 requests
loop(_,N) when N > 10 ->
	ok;
loop(Socket,N) ->
	io:format("Sending request ~B...~n",[N]),
	ok = erlzmq:send(Socket,<<"Hello">>),
	
	{ok,Msg} = erlzmq:recv(Socket),
	io:format("Received reply ~B [ ~s ]~n",[N,Msg]),
	loop(Socket,N+1).

