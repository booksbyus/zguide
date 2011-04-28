%% 
%% Weather update server with sync with subcribers
%% Bind to tcp://*:5561 and tcp://*:5562. After receiving messages from connected subscribers
%% starts publish random weather. 
%% Mikhail Kulemin <mihkulemin@gmail.com>
%%
-module(syncpub).
-export([run/0]).

run() ->
	{ok,Context} = erlzmq:context(),
	{ok,Publisher} = erlzmq:socket(Context,pub),
	ok = erlzmq:bind(Publisher,"tcp://*:5561"),

	{ok,Sync} = erlzmq:socket(Context,rep),
	ok = erlzmq:bind(Sync,"tcp://*:5562"),

	ok = wait_for_clients(Sync,10),
	ok = erlzmq:close(Sync),
	
	loop(Publisher),
	ok = erlzmq:close(Publisher),
	ok = erlzmq:term(Context).

loop(Socket) ->
	loop(Socket,1000000).

loop(Socket,0) ->
	ok = erlzmq:send(Socket,<<"END">>),
	ok;
loop(Socket,N) ->
	ok = erlzmq:send(Socket,<<"Rhubarb">>),
	loop(Socket,N-1).

wait_for_clients(_,0) ->
	ok;
wait_for_clients(Sync,N) ->
	{ok,_} = erlzmq:recv(Sync),
	ok = erlzmq:send(Sync,<<"">>),
	wait_for_clients(Sync,N-1).
