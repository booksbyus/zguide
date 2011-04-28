%% Weather update client

-module(wuclient).
-export([run/0,run/1]).

run() ->
	run("10001").

run(Zip) ->
	{ok,Context} = erlzmq:context(),
	{ok,Socket} = erlzmq:socket(Context,sub),

	io:format("Collecting updates from weather server...",[]),
	ok = erlzmq:connect(Socket,"tcp://localhost:5556"),

	ok = erlzmq:setsockopt(Socket,subscribe,Zip),

	loop(Socket,Zip),
	ok = erlzmq:close(Socket),
	ok = erlzmq:term(Context).

loop(Socket,Zip) ->
	loop(Socket,Zip,0,0).

loop(_,Zip,5,Acc) ->
	io:format("Averange temperature for zipcode ~s was ~BF~n",[Zip,Acc div 5]);
loop(Socket,Zip,N,Acc) -> 
	{ok,Msg} = erlzmq:recv(Socket),

	{ok,[_,Temp,_],_} = io_lib:fread("~d ~d ~d",erlang:binary_to_list(Msg)),
	loop(Socket,Zip,N+1,Acc+Temp).
