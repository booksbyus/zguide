-module(hwclient).
-export([main/0]).

%%
%% "Hello world" client example.
%% Connects to tcp://localhost:5555
%% Sends <<"Hello">> to server and prints the response
%%

main() ->
    application:start(chumak),
    {ok, Socket} = chumak:socket(req, "my-req"),
    {ok, Pid} = chumak:connect(Socket, tcp, "localhost", 5555),
    loop(Socket).


loop(Socket) ->
    chumak:send(Socket, "Hello"),
    {ok, RecvMessage} = chumak:recv(Socket),
    io:format("Recv Reply: ~p\n", [RecvMessage]),
    loop(Socket).
