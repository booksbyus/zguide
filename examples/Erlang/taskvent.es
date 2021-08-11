#! /usr/bin/env escript
%%
%% Task ventilator
%% Binds PUSH socket to tcp://localhost:5557
%% Sends batch of tasks to workers via that socket
%%
main(_Args) ->
    application:start(chumak),
    {ok, Sender} = chumak:socket(push),
    
    case chumak:bind(Sender, tcp, "*", 5557) of
        {ok, _BindPid} ->
            io:format("Binding OK with Pid: ~p\n", [Sender]);
        {error, Reason} ->
            io:format("Connection Failed for this reason: ~p\n", [Reason]);
        X ->
            io:format("Unhandled reply for bind ~p\n", [X])
    end,

    {ok, Sink} = chumak:socket(push),

    case chumak:connect(Sink, tcp, "localhost", 5558) of
        {ok, _ConnectPid} ->
            io:format("Connection OK with Pid: ~p\n", [Sink]);
        {error, Reason2} ->
            io:format("Connection Failed for this reason: ~p\n", [Reason2]);
        X2 ->
            io:format("Unhandled reply for connect  ~p\n", [X2])
    end,

    {ok, _} = io:fread("Press Enter when workers are ready: ", ""),
    io:format("Sending task to workers~n", []),

    ok = chumak:send(Sink, <<"0">>),

    %% Send 100 tasks
    TotalCost = send_tasks(Sender, 100, 0),
    io:format("Total expected cost: ~b msec~n", [TotalCost]).


send_tasks(_Sender, 0, TotalCost) -> TotalCost;
send_tasks(Sender, N, TotalCost) when N>0 ->
    Workload = rand:uniform(100) + 1,
    ok = chumak:send(Sender, list_to_binary(integer_to_list(Workload))),
    send_tasks(Sender, N-1, TotalCost + Workload).
