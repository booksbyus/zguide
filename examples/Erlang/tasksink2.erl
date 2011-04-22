%% 
%% Task sink
%% Binds to tcp://*:5558 and tcp://*:5559. Receives results from workers and send kill signal when job finished
%% Mikhail Kulemin <mihkulemin@gmail.com>
%%

-module(tasksink2).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	{ok,Socket} = erlzmq:socket(Context,pull),
	ok = erlzmq:bind(Socket,"tcp://*:5558"),

	{ok,Controller} = erlzmq:socket(Context,pub),
	ok = erlzmq:bind(Controller,"tcp://*:5559"),

	{ok,_} = erlzmq:recv(Socket),
	Start = now(),
	
	loop(Socket,0,Start,Controller),
	
	ok = erlzmq:close(Controller),	
	ok = erlzmq:close(Socket),
	ok = erlzmq:term(Context).

loop(_,100,Start,Controller) ->
	io:format("~nTotal elapsed time: ~B msec~n",[timer:now_diff(now(),Start) div 1000]),
	ok = erlzmq:send(Controller,<<"KILL">>);
loop(Socket,N,Start,Controller) ->
	{ok,_} = erlzmq:recv(Socket),
	
	Symb = case (N+1) rem 10 of
		0 -> ":";
		_ -> "."
	end,
	io:format("~s",[Symb]),
	loop(Socket,N+1,Start,Controller).




