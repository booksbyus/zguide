-module(hwserver).
-export([main/0]).

%% Starts a local hello server.

main() ->
    application:start(chumak),
    {ok, Socket} = chumak:socket(rep, "my-rep"),
    {ok, Pid} = chumak:bind(Socket, tcp, "localhost", 5555),
    loop(Socket).

loop(Socket) ->
    {ok, RecvMessage} = chumak:recv(Socket),
    io:format("Received request : ~p\n", [RecvMessage]),

    timer:sleep(1000),

    chumak:send(Socket, "World"),
    loop(Socket).
