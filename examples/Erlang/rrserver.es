#! /usr/bin/env escript
%%
%% Hello World server
%% Connects REP socket to tcp://*:5560
%% Expects "Hello" from client, replies with "World"
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to talk to clients
    {ok, Responder} = erlzmq:socket(Context, rep),
    ok = erlzmq:connect(Responder, "tcp://*:5560"),

    loop(Responder),

    %% We never get here but clean up anyhow
    ok = erlzmq:close(Responder),
    ok = erlzmq:term(Context).

loop(Socket) ->
    %% Wait for next request from client
    {ok, Req} = erlzmq:recv(Socket),
    io:format("Received request: [~s]~n", [Req]),

    %% Do some 'work'
    timer:sleep(1000),

    %% Send reply back to client
    ok = erlzmq:send(Socket, <<"World">>),

    loop(Socket).
