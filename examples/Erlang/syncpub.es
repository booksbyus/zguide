#! /usr/bin/env escript
%%
%% Synchronized publisher
%%

%% We wait for 10 subscribers
-define(SUBSCRIBERS_EXPECTED, 10).

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to talk to clients
    {ok, Publisher} = erlzmq:socket(Context, pub),
    ok = erlzmq:bind(Publisher, "tcp://*:5561"),

    %% Socket to receive signals
    {ok, Syncservice} = erlzmq:socket(Context, rep),
    ok = erlzmq:bind(Syncservice, "tcp://*:5562"),

    %% Get synchronization from subscribers
    io:format("Waiting for subscribers~n"),
    sync_subscribers(Syncservice, ?SUBSCRIBERS_EXPECTED),

    %% Now broadcast exactly 1M updates followed by END
    io:format("Broadcasting messages~n"),
    broadcast(Publisher, 1000000),

    ok = erlzmq:send(Publisher, <<"END">>),

    ok = erlzmq:close(Publisher),
    ok = erlzmq:close(Syncservice),
    ok = erlzmq:term(Context).

sync_subscribers(_Syncservice, 0) -> ok;
sync_subscribers(Syncservice, N) when N > 0 ->
    %% Wait for synchornization request
    {ok, _} = erlzmq:recv(Syncservice),
    %% Send synchronization reply
    ok = erlzmq:send(Syncservice, <<>>),
    sync_subscribers(Syncservice, N - 1).

broadcast(_Publisher, 0) -> ok;
broadcast(Publisher, N) when N > 0 ->
    ok = erlzmq:send(Publisher, <<"Rhubarb">>),
    broadcast(Publisher, N - 1).
