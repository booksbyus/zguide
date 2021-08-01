#! /usr/bin/env escript

%% Starts a local hello server.
%% Binds to tcp://localhost:5555

main(_Args) ->
    application:start(chumak),
    {ok, Socket} = chumak:socket(rep, "my-rep"),
    {ok, _Pid} = chumak:bind(Socket, tcp, "localhost", 5555),
    loop(Socket).

loop(Socket) ->
    {ok, RecvMessage} = chumak:recv(Socket),
    io:format("Received request : ~p\n", [RecvMessage]),

    timer:sleep(1000),

    chumak:send(Socket, "World"),
    loop(Socket).
