#! /usr/bin/env escript
%%
%% Publisher for durable subscriber
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %%  Subscriber tells us when it's ready here
    {ok, Sync} = erlzmq:socket(Context, pull),
    ok = erlzmq:bind(Sync, "tcp://*:5564"),

    %%  We send updates via this socket
    {ok, Publisher} = erlzmq:socket(Context, pub),
    ok = erlzmq:bind(Publisher, "tcp://*:5565"),

    %%  Wait for synchronization request
    {ok, _} = erlzmq:recv(Sync),

    %%  Now broadcast exactly 10 updates with pause
    lists:foreach(
      fun(Num) ->
              Msg = list_to_binary(io_lib:format("Update ~b", [Num])),
              ok = erlzmq:send(Publisher, Msg),
              timer:sleep(1000)
      end, lists:seq(1, 10)),
    erlzmq:send(Publisher, <<"END">>),

    erlzmq:close(Sync),
    erlzmq:close(Publisher),
    erlzmq:term(Context).
