%%
%% "Hello world" server example.
%% Binds socket to tcp://*:5555 and waiting for request
%% Reply <<"World">> to any request
%% Mikhail Kulemin <mihkulemin@gmail.com>
%%
-module(hwserver).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	{ok,Socket} = erlzmq:socket(Context,rep),
	ok = erlzmq:bind(Socket,"tcp://*:5555"),

	loop(Socket),

	%% Loop is infinite but...
	ok = erlzmq:close(Socket),
	ok = erlzmq:term(Context).

loop(Socket) ->
	% Wait for request
	{ok,Msg} = erlzmq:recv(Socket),
	io:format("Received request: ~s~n",[Msg]),

	% Some work
	timer:sleep(1000),

	% Send reply
	ok = erlzmq:send(Socket,<<"World">>),
	loop(Socket).

