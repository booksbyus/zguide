#! /usr/bin/env escript
%%
%% Weather update server
%% Binds PUB socket to tcp://*.5556
%% Publishes random weather updates
%%

main(_) ->
    %% Prepare our context and publisher
    {ok, Context} = erlzmq:context(),
    {ok, Publisher} = erlzmq:socket(Context, pub),
    ok = erlzmq:bind(Publisher, "tcp://*:5556"),

    loop(Publisher),

    %% We never get here
    ok = erlzmq:close(Publisher),
    ok = erlzmq:term(Context).

loop(Publisher) ->
    %% Get values that will fool the boss
    Zipcode = random:uniform(100000),
    Temperature = random:uniform(215) - 80,
    Relhumidity = random:uniform(50) + 10,

    %% Send message to all subscribers
    Msg = list_to_binary(
            io_lib:format("~5..0b ~b ~b",
                          [Zipcode, Temperature, Relhumidity])),
    ok = erlzmq:send(Publisher, Msg),

    loop(Publisher).
