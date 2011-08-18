#! /usr/bin/env escript
%%
%% Pubsub envelope subscriber
%%

main(_) ->
    %% Prepare our context and subscriber
    {ok, Context} = erlzmq:context(),
    {ok, Subscriber} = erlzmq:socket(Context, sub),
    ok = erlzmq:connect(Subscriber, "tcp://localhost:5563"),
    ok = erlzmq:setsockopt(Subscriber, subscribe, <<"B">>),

    loop(Subscriber),

    %% We never get here but clean up anyhow
    ok = erlzmq:close(Subscriber),
    ok = erlzmq:term(Context).

loop(Subscriber) ->
    %% Read envelope with address
    {ok, Address} = erlzmq:recv(Subscriber),
    %% Read message contents
    {ok, Contents} = erlzmq:recv(Subscriber),
    io:format("[~s] ~s~n", [Address, Contents]),
    loop(Subscriber).
