#!/usr/bin/env escript
%%
%% Multiprocess Hello World server (analogous to C threads example)
%%

worker_routine(Context) ->
    %% Socket to talk to dispatcher
    {ok, Receiver} = erlzmq:socket(Context, rep),
    ok = erlzmq:connect(Receiver, "inproc://workers"),
    worker_loop(Receiver),
    ok = erlzmq:close(Receiver).

worker_loop(Receiver) ->
    {ok, Msg} = erlzmq:recv(Receiver),
    io:format("Received ~s [~p]~n", [Msg, self()]),
    %% Do some work
    timer:sleep(1000),
    erlzmq:send(Receiver, <<"World">>),
    worker_loop(Receiver).

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to talk to clients
    {ok, Clients} = erlzmq:socket(Context, [router, {active, true}]),
    ok = erlzmq:bind(Clients, "tcp://*:5555"),

    %% Socket to talk to workers
    {ok, Workers} = erlzmq:socket(Context, [dealer, {active, true}]),
    ok = erlzmq:bind(Workers, "inproc://workers"),

    %% Start worker processes
    start_workers(Context, 5),

    %% Connect work threads to client threads via a queue
    erlzmq_device:queue(Clients, Workers),

    %% We never get here but cleanup anyhow
    ok = erlzmq:close(Clients),
    ok = erlzmq:close(Workers),
    ok = erlzmq:term(Context).

start_workers(_Context, 0) -> ok;
start_workers(Context, N) when N > 0 ->
    spawn(fun() -> worker_routine(Context) end),
    start_workers(Context, N - 1).
