%%
%% Weather update client with sync
%% Connects to tcp://localhost:5561 and tcp://localhost:5562 for sync 

-module(syncsub).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	{ok,Subscriber} = erlzmq:socket(Context,sub),
	{ok,Syncclient} = erlzmq:socket(Context,req),


	ok = erlzmq:connect(Subscriber,"tcp://localhost:5561"),
	ok = erlzmq:setsockopt(Subscriber,subscribe,""),

	ok = erlzmq:connect(Syncclient,"tcp://localhost:5562"),

	ok = erlzmq:send(Syncclient,<<"">>),
	{ok,_} = erlzmq:recv(Syncclient),

	loop(Subscriber),
	ok = erlzmq:close(Subscriber),
	ok = erlzmq:term(Context).

loop(ok) ->
	ok;
loop(Sub) ->
	{ok,Msg} = erlzmq:recv(Sub),
	Next = case Msg of
		<<"END">> ->
			ok;
		_ -> 
			Sub
	end,
	loop(Next).
