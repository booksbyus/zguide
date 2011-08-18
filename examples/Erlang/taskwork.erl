#! /usr/bin/env escript
%%
%% Task worker
%% Connects PULL socket to tcp://localhost:5557
%% Collects workloads from ventillator via that socket
%% Connects PUSH socket to tcp://localhost:5558
%% Sends results to sink via that socket
%%

main(_) ->
    {ok,Context} = erlzmq:context(),

    %% Socket to receive messages on
    {ok ,Receiver} = erlzmq:socket(Context, pull),
    ok = erlzmq:connect(Receiver, "tcp://localhost:5557"),

    %% Socket to send messages to
    {ok, Sender} = erlzmq:socket(Context, push),
    ok = erlzmq:connect(Sender, "tcp://localhost:5558"),

    %% Process tasks forever
    loop(Receiver,Sender),

    %% We never get here, but
    ok = erlzmq:close(Receiver),
    ok = erlzmq:close(Sender),
    ok = erlzmq:term(Context).

loop(Receiver,Sender) ->
    {ok, Work} = erlzmq:recv(Receiver),

    %% Simple progress indicator for the viewer
    io:format("."),

    %% Do the work
    timer:sleep(list_to_integer(binary_to_list(Work))),

    %% Send results to sink
    ok = erlzmq:send(Sender, <<"">>),

    loop(Receiver, Sender).
