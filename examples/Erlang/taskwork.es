#! /usr/bin/env escript
%%
%% Task worker
%% Connects PULL socket to tcp://localhost:5557
%% Collects workloads from ventillator via that socket
%% Connects PUSH socket to tcp://localhost:5558
%% Sends results to sink via that socket
%%

main(_) ->
    application:start(chumak),

    {ok, Receiver} = chumak:socket(pull),

    case chumak:connect(Receiver, tcp, "localhost", 5557) of
        {ok, _ConnectPid} ->
            io:format("Connection OK with Pid: ~p\n", [Receiver]);
        {error, Reason} ->
            io:format("Connection failed for this reason: ~p\n", [Reason])
    end,

    {ok, Sender} = chumak:socket(push),
    case chumak:connect(Sender, tcp, "localhost", 5558) of
        {ok, _ConnectPid1} ->
            io:format("Connection OK with Pid: ~p\n", [Sender])
    end,

    loop(Receiver, Sender).

loop(Receiver, Sender) ->
    {ok, Work} = chumak:recv(Receiver),

    io:format(" . "),

    timer:sleep(list_to_integer(binary_to_list(Work))),

    ok = chumak:send(Sender, <<" ">>),
    loop(Receiver, Sender).
