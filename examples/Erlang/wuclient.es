#! /usr/bin/env escript
%%
%% Weather update client
%% Connects SUB socket to tcp://localhost:5556
%% Collects weather updates and fins avg temp in zipcode
%%

main(Args) ->
    application:start(chumak),
    {ok, Subscriber} = chumak:socket(sub),

    %% select default topic or from the args
    Topic = case Args of
            [] -> <<"10001">>;
            [Arg1 | _] -> list_to_binary(Arg1)
        end,
    io:format("Collecting updates from weather server...~n"),
    io:format("For zipcode: ~p\n", [Topic]),
    chumak:subscribe(Subscriber, Topic),
    case chumak:connect(Subscriber, tcp, "localhost", 5556) of
        {ok, _BindPid} ->
            io:format("Binding OK with Pid: ~p\n", [Subscriber]);
        {error, Reason} ->
            io:foramt("Connection failed for this reason: ~p\n", [Reason]);
        X ->
            io:format("Unhandled reply for bind ~p \n", [X])
    end,

    N = 10, %% number of records to collect
    TotalTemp = collect_temperature(Subscriber, N, 0),
    io:format("Average Temperature is ~p\n", [TotalTemp/N]).


collect_temperature(_Subscriber, 0, Total) -> Total;
collect_temperature(Subscriber, N, Total) when N > 0 ->
    {ok, Data} = chumak:recv(Subscriber),
    io:format("RECEIVED : ~p\n", [Data]),
    [_, Temp, _] = string:split(Data, " ", all),
    IntTemp = erlang:binary_to_integer(Temp),
    collect_temperature(Subscriber, N-1, Total + IntTemp).
