%%
%% Reading from multiple sockets using active sockets
%% Mikhail Kulemin <mihkulemin@gmail.com>
%%

-module(mspolling).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	
	{ok,Receiver} = erlzmq:socket(Context,[pull,{active,true}]),
	ok = erlzmq:connect(Receiver,"tcp://localhost:5557"),

	{ok,Subscriber} = erlzmq:socket(Context,[sub,{active,true}]),
	ok = erlzmq:connect(Subscriber,"tcp://localhost:5556"),
	ok = erlzmq:setsockopt(Subscriber,subscribe,"10001"),

	loop(Receiver,Subscriber),

	ok = erlzmq:close(Receiver),
	ok = erlzmq:close(Subscriber),
	ok = erlzmq:term(Context).

loop(Sub,Recv) ->
	receive 
		{zmq,Sub,Msg} ->
			io:format("Msg from SUB: ~s~n",[Msg]),
			loop(Sub,Recv);
		{zmq,Recv,Msg} ->
			io:format("Msg from PULL: ~s~n",[Msg]),
			loop(Sub,Recv)
	end.


