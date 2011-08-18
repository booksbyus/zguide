#! /usr/bin/env escript
%%
%% Hello World server in Erlang
%% Binds REP socket to tcp://*:5555
%% Accepts any message, replies with "World"
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to talk to clients
    {ok, Responder} = erlzmq:socket(Context, rep),
    ok = erlzmq:bind(Responder, "tcp://*:5555"),

    loop(Responder),

    %% We never get here, but it we did, this is how we end
    ok = erlzmq:close(Responder),
    ok = erlzmq:term(Context).

loop(Responder) ->
    %% Wait for request
    {ok, Msg} = erlzmq:recv(Responder),
    io:format("Received ~s~n", [Msg]),

    %% Do some 'work'
    timer:sleep(1000),

    %% Send reply back to client
    ok = erlzmq:send(Responder, <<"World">>),
    loop(Responder).
