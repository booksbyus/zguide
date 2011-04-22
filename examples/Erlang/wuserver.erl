%% 
%% Weather update server
%% Bind to tcp://*:5556 and publishes random weather 
%% Mikhail Kulemin <mihkulemin@gmail.com>
%%
-module(wuserver).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	{ok,Socket} = erlzmq:socket(Context,pub),
	ok = erlzmq:bind(Socket,"tcp://*:5556"),

	loop(Socket).

loop(Socket) ->
	Zipcode = random:uniform(100000),
	Temp = random:uniform(215) - 80,
	Relhum = random:uniform(50) + 10,

	Msg = erlang:list_to_binary(io_lib:format("~B ~B ~B",[Zipcode,Temp,Relhum])),
	ok = erlzmq:send(Socket,Msg),
	
	loop(Socket).
