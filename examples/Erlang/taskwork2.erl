%%
%% Task worker
%% Connects to ventilator on tcp://localhost:5557, sink on tcp://localhost:5558 and subscribe to tcp://localhost:5559
%% Receives task and push result to sink. Terminate when receives <<"KILL">> message from sink
%% Mikhail Kulemin <mihkulemin@gmail.com>
%%

-module(taskwork2).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	
	{ok,Receiver} = erlzmq:socket(Context,[pull,{active,true}]),
	ok = erlzmq:connect(Receiver,"tcp://localhost:5557"),

	{ok,Sender} = erlzmq:socket(Context,push),
	ok = erlzmq:connect(Sender,"tcp://localhost:5558"),

	{ok,Controller} = erlzmq:socket(Context,[sub,{active,true}]),
	ok = erlzmq:connect(Controller,"tcp://localhost:5559"),
	ok = erlzmq:setsockopt(Controller,subscribe,""),



	ok = loop(Receiver,Sender,Controller),

	ok = erlzmq:close(Receiver),
	ok = erlzmq:close(Sender),
	ok = erlzmq:close(Controller),
	ok = erlzmq:term(Context).

loop(Receiver,Sender,Controller) ->
	receive 
		{zmq,Receiver,Work} ->
			io:format(".",[]),

			Time = list_to_integer(binary_to_list(Work)),
			timer:sleep(Time),

			ok = erlzmq:send(Sender,<<"">>),
			loop(Receiver,Sender,Controller);
		{zmq,Controller,<<"KILL">>} ->
			ok
	end.	
	

