#! /usr/bin/env escript
%%
%% Task sink - design 2
%% Adds pub-sub flow to send kill signal to workers
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to receive messages on
    {ok, Receiver} = erlzmq:socket(Context, pull),
    ok = erlzmq:bind(Receiver, "tcp://*:5558"),

    %% Socket for worker control
    {ok, Controller} = erlzmq:socket(Context, pub),
    ok = erlzmq:bind(Controller, "tcp://*:5559"),

    %% Wait for start of batch
    {ok, _} = erlzmq:recv(Receiver),

    %% Start our clock now
    Start = now(),

    %% Process 100 confirmations
    process_confirmations(Receiver, 100),
    io:format("Total elapsed time: ~b msec~n",
              [timer:now_diff(now(), Start) div 1000]),

    %% Send kill signal to workers
    ok = erlzmq:send(Controller, <<"KILL">>),

    %% Finished
    ok = erlzmq:close(Controller),
    ok = erlzmq:close(Receiver),
    ok = erlzmq:term(Context, 1000).

process_confirmations(_Receiver, 0) -> ok;
process_confirmations(Receiver, N) when N > 0 ->
    {ok, _} = erlzmq:recv(Receiver),
    case N - 1 rem 10 of
        0 -> io:format(":");
        _ -> io:format(".")
    end,
    process_confirmations(Receiver, N - 1).
