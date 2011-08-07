#! /usr/bin/env escript
%%
%% Reading from multiple sockets
%% This version uses a simple recv loop
%%

main(_) ->
    %% Prepare our context and sockets
    {ok, Context} = erlzmq:context(),

    %% Connect to task ventilator
    {ok, Receiver} = erlzmq:socket(Context, pull),
    ok = erlzmq:connect(Receiver, "tcp://localhost:5557"),

    %% Connect to weather server
    {ok, Subscriber} = erlzmq:socket(Context, sub),
    ok = erlzmq:connect(Subscriber, "tcp://localhost:5556"),
    ok = erlzmq:setsockopt(Subscriber, subscribe, <<"10001">>),

    %% Process messages from both sockets
    loop(Receiver, Subscriber),

    %% We never get here but clean up anyhow
    ok = erlzmq:close(Receiver),
    ok = erlzmq:close(Subscriber),
    ok = erlzmq:term(Context).

loop(Receiver, Subscriber) ->
    %% We prioritize traffic from the task ventilator
    process_tasks(Receiver),
    process_weather(Subscriber),
    timer:sleep(1000),
    loop(Receiver, Subscriber).

process_tasks(S) ->
    %% Process any waiting tasks
    case erlzmq:recv(S, [noblock]) of
        {error, eagain} -> ok;
        {ok, Msg} ->
            io:format("Procesing task: ~s~n", [Msg]),
            process_tasks(S)
    end.

process_weather(S) ->
    %% Process any waiting weather updates
    case erlzmq:recv(S, [noblock]) of
        {error, eagain} -> ok;
        {ok, Msg} ->
            io:format("Processing weather update: ~s~n", [Msg]),
            process_weather(S)
    end.
