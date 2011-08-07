#! /usr/bin/env escript
%%
%% "Hello world" client example.
%% Connects to tcp://localhost:5555
%% Sends <<"Hello">> to server and prints the response
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to talk to server
    io:format("Connecting to hello world server...~n"),
    {ok, Requester} = erlzmq:socket(Context, req),
    ok = erlzmq:connect(Requester,"tcp://localhost:5555"),

    %% Send 10 requests
    lists:foreach(
      fun(N) ->
              io:format("Sending Hello ~b...~n", [N]),
              ok = erlzmq:send(Requester, <<"Hello">>),

              {ok, Reply} = erlzmq:recv(Requester),
              io:format("Received ~s ~b~n", [Reply, N])
      end, lists:seq(1, 10)),

    ok = erlzmq:close(Requester),
    ok = erlzmq:term(Context).
