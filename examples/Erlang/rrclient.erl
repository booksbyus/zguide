#! /usr/bin/env escript
%%
%% Hello World client
%% Connects REQ socket to tcp://localhost:5559
%% Sends "Hello" to server, expects "World" back
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to talk to server
    {ok, Requester} = erlzmq:socket(Context, req),
    ok = erlzmq:connect(Requester, "tcp://*:5559"),

    lists:foreach(
      fun(Num) ->
              erlzmq:send(Requester, <<"Hello">>),
              {ok, Reply} = erlzmq:recv(Requester),
              io:format("Received reply ~b [~s]~n", [Num, Reply])
      end, lists:seq(1, 10)),

    ok = erlzmq:close(Requester),
    ok = erlzmq:term(Context).
