#! /usr/bin/env escript
%%
%% Durable subscriber
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %%  Connect our subscriber socket
    {ok, Subscriber} = erlzmq:socket(Context, sub),
    ok = erlzmq:setsockopt(Subscriber, identity, <<"Hello">>),
    ok = erlzmq:setsockopt(Subscriber, subscribe, <<>>),
    ok = erlzmq:connect(Subscriber, "tcp://localhost:5565"),

    %%  Synchronize with publisher
    {ok, Sync} = erlzmq:socket(Context, push),
    ok = erlzmq:connect(Sync, "tcp://localhost:5564"),
    ok = erlzmq:send(Sync, <<>>),

    %%  Get updates, exit when told to do so
    loop(Subscriber),

    ok = erlzmq:close(Sync),
    ok = erlzmq:close(Subscriber),
    ok = erlzmq:term(Context).

loop(Subscriber) ->
    case erlzmq:recv(Subscriber) of
        {ok, <<"END">>} -> ok;
        {ok, Msg} ->
            io:format("~s~n", [Msg]),
            loop(Subscriber)
    end.
