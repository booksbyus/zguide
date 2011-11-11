#! /usr/bin/env escript
%%
%% Task ventilator
%% Binds PUSH socket to tcp://localhost:5557
%% Sends batch of tasks to workers via that socket
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to send messages on  
    {ok, Sender} = erlzmq:socket(Context, push),
    ok = erlzmq:bind(Sender, "tcp://*:5557"),

    %% Socket to send start of batch message on
    {ok, Sink} = erlzmq:socket(Context, push),
    ok = erlzmq:connect(Sink, "tcp://localhost:5558"),

    {ok, _} = io:fread("Press Enter when workers are ready: ", ""),
    io:format("Sending task to workers~n",[]),

    %% The first message is "0" and signals start of batch
    ok = erlzmq:send(Sink, <<"0">>),

    %% Send 100 tasks
    TotalCost = send_tasks(Sender, 100, 0),
    io:format("Total expected cost: ~b msec~n", [TotalCost]),

    ok = erlzmq:close(Sink),
    ok = erlzmq:close(Sender),

    %% Terminate with 1 second to send pending messages
    erlzmq:term(Context, 1000).

send_tasks(_Sender, 0, TotalCost) -> TotalCost;
send_tasks(Sender, N, TotalCost) when N > 0 ->
    Workload = random:uniform(100) + 1,
    ok = erlzmq:send(Sender, list_to_binary(integer_to_list(Workload))),
    send_tasks(Sender, N - 1, TotalCost + Workload).
