%% Task ventilator
-module(taskvent).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	{ok,Socket} = erlzmq:socket(Context,push),
	ok = erlzmq:bind(Socket,"tcp://*:5557"),

	{ok,_} = io:fread("Press Enter when workers are ready: ",""),
	io:format("Sending task to workers~n",[]),

	ok = erlzmq:send(Socket,<<"0">>),
	
	loop(Socket,0,0),
	ok = erlzmq:close(Socket),
	ok = erlzmq:term(Context).

loop(_,_Totaltime,100) ->
	io:format("Total expected cost: ~B msec~n",[_Totaltime]);
loop(Socket,Totaltime,N) ->
	Time = random:uniform(100),

	ok = erlzmq:send(Socket,list_to_binary(integer_to_list(Time))),
	loop(Socket,Totaltime+Time,N+1).
