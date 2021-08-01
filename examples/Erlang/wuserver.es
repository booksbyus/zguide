#! /usr/bin/env escript
%%
%% Weather update server
%% Binds PUB socket to tcp://*.5556
%% Publishes random weather updates
%%

main(_Args) ->
    application:start(chumak),
    {ok, Publisher} = chumak:socket(pub),

    case chumak:bind(Publisher, tcp, "localhost", 5556) of
        {ok, _BindPid} ->
            io:format("Binding OK with Pid: ~p\n", [Publisher]);
        {error, Reason} ->
            io:format("Connection Failed for this reason: ~p\n", [Reason]);
        X ->
            io:format("Unhandled reply for bind ~p \n", [X])
    end,
    loop(Publisher).


loop(Publisher) ->
    Zipcode = rand:uniform(100000),
    Temperature = rand:uniform(135),
    Relhumidity = rand:uniform(50) + 10,

    BinZipCode = erlang:integer_to_binary(Zipcode),
    BinTemperature = erlang:integer_to_binary(Temperature),
    BinRelhumidity = erlang:integer_to_binary(Relhumidity),
    ok = chumak:send(Publisher, [BinZipCode, " ", BinTemperature, " ", BinRelhumidity]),
    loop(Publisher).
