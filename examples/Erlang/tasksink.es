#! /usr/bin/env escript
%%
%% Task sink
%% Binds PULL socket to tcp://localhost:5558
%% Collects results from workers via that socket
%%

main(_) ->
    %% Prepare our context and socket
    {ok, Context} = erlzmq:context(),
    {ok, Receiver} = erlzmq:socket(Context, pull),
    ok = erlzmq:bind(Receiver, "tcp://*:5558"),

    %% Wait for start of batch
    {ok, _} = erlzmq:recv(Receiver),

    %% Start our clock now
    Start = now(),

    %% Process 100 confirmations
    process_confirmations(Receiver, 100),

    %% Calculate and report duration of batch
    io:format("Total elapsed time: ~b msec~n",
              [timer:now_diff(now(), Start) div 1000]),

    ok = erlzmq:close(Receiver),
    ok = erlzmq:term(Context).

process_confirmations(_Receiver, 0) -> ok;
process_confirmations(Receiver, N) when N > 0 ->
    {ok, _} = erlzmq:recv(Receiver),
    case N - 1 rem 10 of
        0 -> io:format(":");
        _ -> io:format(".")
    end,
    process_confirmations(Receiver, N - 1).

