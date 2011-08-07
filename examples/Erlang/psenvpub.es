#! /usr/bin/env escript
%%
%% Pubsub envelope publisher
%%

main(_) ->
    %% Prepare our context and publisher
    {ok, Context} = erlzmq:context(),
    {ok, Publisher} = erlzmq:socket(Context, pub),
    ok = erlzmq:bind(Publisher, "tcp://*:5563"),

    loop(Publisher),

    %% We never get here but clean up anyhow
    ok = erlzmq:close(Publisher),
    ok = erlzmq:term(Context).

loop(Publisher) ->
    %% Write two messages, each with an envelope and content
    ok = erlzmq:send(Publisher, <<"A">>, [sndmore]),
    ok = erlzmq:send(Publisher, <<"We don't want to see this">>),
    ok = erlzmq:send(Publisher, <<"B">>, [sndmore]),
    ok = erlzmq:send(Publisher, <<"We would like to see this">>),
    timer:sleep(1000),
    loop(Publisher).
