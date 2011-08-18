#! /usr/bin/env escript
%%
%% Synchronized subscriber
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% First, connect our subscriber socket
    {ok, Subscriber} = erlzmq:socket(Context, sub),
    ok = erlzmq:connect(Subscriber, "tcp://localhost:5561"),
    ok = erlzmq:setsockopt(Subscriber, subscribe, <<>>),

    %% Second, synchronize with publisher
    {ok, Syncclient} = erlzmq:socket(Context, req),
    ok = erlzmq:connect(Syncclient, "tcp://localhost:5562"),

    %% - send a synchronization request
    ok = erlzmq:send(Syncclient, <<>>),

    %% - wait for synchronization reply
    {ok, <<>>} = erlzmq:recv(Syncclient),

    %% Third, get our updates and report how many we got
    Updates = acc_updates(Subscriber, 0),
    io:format("Received ~b updates~n", [Updates]),

    ok = erlzmq:close(Subscriber),
    ok = erlzmq:close(Syncclient),
    ok = erlzmq:term(Context).

acc_updates(Subscriber, N) ->
    case erlzmq:recv(Subscriber) of
        {ok, <<"END">>} -> N;
        {ok, _} -> acc_updates(Subscriber, N + 1)
    end.
