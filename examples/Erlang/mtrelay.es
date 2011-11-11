#!/usr/bin/env escript
%%
%% Multithreaded relay
%%
%% This example illustrates how inproc sockets can be used to communicate
%% across "threads". Erlang of course supports this natively, but it's fun to
%% see how 0MQ lets you do this across other languages!
%%

step1(Context) ->
    %% Connect to step2 and tell it we're ready
    {ok, Xmitter} = erlzmq:socket(Context, pair),
    ok = erlzmq:connect(Xmitter, "inproc://step2"),
    io:format("Step 1 ready, signaling step 2~n"),
    ok = erlzmq:send(Xmitter, <<"READY">>),
    ok = erlzmq:close(Xmitter).

step2(Context) ->
    %% Bind inproc socket before starting step1
    {ok, Receiver} = erlzmq:socket(Context, pair),
    ok = erlzmq:bind(Receiver, "inproc://step2"),
    spawn(fun() -> step1(Context) end),

    %% Wait for signal and pass it on
    {ok, _} = erlzmq:recv(Receiver),
    ok = erlzmq:close(Receiver),

    %% Connect to step3 and tell it we're ready
    {ok, Xmitter} = erlzmq:socket(Context, pair),
    ok = erlzmq:connect(Xmitter, "inproc://step3"),
    io:format("Step 2 ready, signaling step 3~n"),
    ok = erlzmq:send(Xmitter, <<"READY">>),
    ok = erlzmq:close(Xmitter).

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Bind inproc socket before starting step2
    {ok, Receiver} = erlzmq:socket(Context, pair),
    ok = erlzmq:bind(Receiver, "inproc://step3"),
    spawn(fun() -> step2(Context) end),

    %% Wait for signal
    {ok, _} = erlzmq:recv(Receiver),
    erlzmq:close(Receiver),

    io:format("Test successful~n"),
    ok = erlzmq:term(Context).
