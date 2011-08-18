#! /usr/bin/env escript
%%
%% Weather proxy device
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% This is where the weather server sits
    {ok, Frontend} = erlzmq:socket(Context, sub),
    ok = erlzmq:connect(Frontend, "tcp://localhost:5556"),

    %% This is our public endpoint for subscribers
    {ok, Backend} = erlzmq:socket(Context, pub),
    ok = erlzmq:bind(Backend, "tcp://*:8100"),

    %% Subscribe on everything
    ok = erlzmq:setsockopt(Frontend, subscribe, <<>>),

    %% Shunt messages out to our own subscribers
    loop(Frontend, Backend),

    %% We don't actually get here but if we did, we'd shut down neatly
    ok = erlzmq:close(Frontend),
    ok = erlzmq:close(Backend),
    ok = erlzmq:term(Context).

loop(Frontend, Backend) ->
    {ok, Msg} = erlzmq:recv(Frontend),
    case erlzmq:getsockopt(Frontend, rcvmore) of
        {ok, true} -> erlzmq:send(Backend, Msg, [sndmore]);
        {ok, false} -> erlzmq:send(Backend, Msg)
    end,
    loop(Frontend, Backend).
