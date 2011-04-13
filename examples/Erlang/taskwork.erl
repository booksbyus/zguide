% Task worker

-module(taskwork).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	
	{ok,Receiver} = erlzmq:socket(Context,pull),
	ok = erlzmq:connect(Receiver,"tcp://localhost:5557"),

	{ok,Sender} = erlzmq:socket(Context,push),
	ok = erlzmq:connect(Sender,"tcp://localhost:5558"),

	loop(Receiver,Sender),

	ok = erlzmq:close(Receiver),
	ok = erlzmq:close(Sender),
	ok = erlzmq:term(Context).

loop(Receiver,Sender) ->
	{ok,Work} = erlzmq:recv(Receiver),
	
	io:format(".",[]),

	Time = list_to_integer(binary_to_list(Work)),
	timer:sleep(Time),
	
	ok = erlzmq:send(Sender,<<"">>),

	loop(Receiver,Sender).
