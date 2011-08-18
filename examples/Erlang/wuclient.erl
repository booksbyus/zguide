#! /usr/bin/env escript
%%
%% Weather update client
%% Connects SUB socket to tcp://localhost:5556
%% Collects weather updates and fins avg temp in zipcode
%%

main(Args) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to talk to server
    io:format("Collecting updates from weather server...~n"),
    {ok, Subscriber} = erlzmq:socket(Context, sub),
    ok = erlzmq:connect(Subscriber, "tcp://localhost:5556"),

    %% Subscribe to zipcode, default is NYC, 10001
    Filter = case Args of
                 [] -> <<"10001">>;
                 [Arg1|_] -> list_to_binary(Arg1)
             end,
    ok = erlzmq:setsockopt(Subscriber, subscribe, Filter),

    %% Process 5 updates (Erlang server is slow relative to C)
    UpdateNbr = 5,
    TotalTemp = collect_temperature(Subscriber, UpdateNbr, 0),

    io:format("Average temperature for zipcode '~s' was ~bF~n",
              [Filter, trunc(TotalTemp / UpdateNbr)]),

    ok = erlzmq:close(Subscriber),
    ok = erlzmq:term(Context).

collect_temperature(_Subscriber, 0, Total) -> Total;
collect_temperature(Subscriber, N, Total) when N > 0 ->
    {ok, Msg} = erlzmq:recv(Subscriber),
    collect_temperature(Subscriber, N - 1, Total + msg_temperature(Msg)).

msg_temperature(Msg) ->
    {ok, [_, Temp, _], _} = io_lib:fread("~d ~d ~d", binary_to_list(Msg)),
    Temp.
