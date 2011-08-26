#! /usr/bin/env escript
%%
%% Reading from multiple sockets
%% This version uses active sockets
%%

main(_) ->
    {ok,Context} = erlzmq:context(),

    %% Connect to task ventilator
    {ok, Receiver} = erlzmq:socket(Context, [pull, {active, true}]),
    ok = erlzmq:connect(Receiver, "tcp://localhost:5557"),

    %% Connect to weather server
    {ok, Subscriber} = erlzmq:socket(Context, [sub, {active, true}]),
    ok = erlzmq:connect(Subscriber, "tcp://localhost:5556"),
    ok = erlzmq:setsockopt(Subscriber, subscribe, <<"10001">>),

    %% Process messages from both sockets
    loop(Receiver, Subscriber),

    %% We never get here
    ok = erlzmq:close(Receiver),
    ok = erlzmq:close(Subscriber),
    ok = erlzmq:term(Context).

loop(Tasks, Weather) ->
    receive
        {zmq, Tasks, Msg, _Flags} ->
            io:format("Processing task: ~s~n",[Msg]),
            loop(Tasks, Weather);
        {zmq, Weather, Msg, _Flags} ->
            io:format("Processing weather update: ~s~n",[Msg]),
            loop(Tasks, Weather)
    end.
