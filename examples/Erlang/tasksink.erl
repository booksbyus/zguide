%% 
%% Task sink
%% Binds to tcp://*:5558 and receives results from workers
%% Mikhail Kulemin <mihkulemin@gmail.com>
%%

-module(tasksink).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	{ok,Socket} = erlzmq:socket(Context,pull),
	ok = erlzmq:bind(Socket,"tcp://*:5558"),

	{ok,_} = erlzmq:recv(Socket),
	Start = now(),
	
	loop(Socket,0,Start),
	
	ok = erlzmq:close(Socket),
	ok = erlzmq:term(Context).

loop(_,100,Start) ->
	io:format("~nTotal elapsed time: ~B msec~n",[timer:now_diff(now(),Start) div 1000]);
loop(Socket,N,Start) ->
	{ok,_} = erlzmq:recv(Socket),
	
	Symb = case (N+1) rem 10 of
		0 -> ":";
		_ -> "."
	end,
	io:format("~s",[Symb]),
	loop(Socket,N+1,Start).




