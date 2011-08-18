#!/usr/bin/env escript
%%
%%  Asynchronous client-to-server (DEALER to ROUTER)
%%
%%  While this example runs in a single process, that is just to make
%%  it easier to start and stop the example. Each task has its own
%%  context and conceptually acts as a separate process.

%%  ---------------------------------------------------------------------
%%  This is our client task
%%  It connects to the server, and then sends a request once per second
%%  It collects responses as they arrive, and it prints them out. We will
%%  run several client tasks in parallel, each with a different random ID.

client_task() ->
    {ok, Ctx} = erlzmq:context(),
    {ok, Client} = erlzmq:socket(Ctx, dealer),

    %%  Set identity to make tracing easier
    ok = erlzmq:setsockopt(Client, identity, pid_to_list(self())),
    ok = erlzmq:connect(Client, "tcp://localhost:5570"),

    client_loop(Client, 0),

    ok = erlzmq:term(Ctx).

client_loop(Client, RequestNbr) ->
    %% Tick once per second, pulling in arriving messages (check 100 times
    %% using 10 poll delay for each call)
    client_check_messages(Client, 100, 10),
    Msg = list_to_binary(io_lib:format("request #~b", [RequestNbr])),
    erlzmq:send(Client, Msg),
    client_loop(Client, RequestNbr + 1).

client_check_messages(_Client, 0, _PollDelay) -> ok;
client_check_messages(Client, N, PollDelay) when N > 0 ->
    case erlzmq:recv(Client, [noblock]) of
        {ok, Msg} -> io:format("~s [~p]~n", [Msg, self()]);
        {error, eagain} -> timer:sleep(PollDelay)
    end,
    client_check_messages(Client, N - 1, PollDelay).

%%  ---------------------------------------------------------------------
%%  This is our server task
%%  It uses the multithreaded server model to deal requests out to a pool
%%  of workers and route replies back to clients. One worker can handle
%%  one request at a time but one client can talk to multiple workers at
%%  once.

server_task() ->
    {ok, Ctx} = erlzmq:context(),
    random:seed(now()),

    %%  Frontend socket talks to clients over TCP
    {ok, Frontend} = erlzmq:socket(Ctx, [router, {active, true}]),
    ok = erlzmq:bind(Frontend, "tcp://*:5570"),

    %%  Backend socket talks to workers over inproc
    {ok, Backend} = erlzmq:socket(Ctx, [dealer, {active, true}]),
    ok = erlzmq:bind(Backend, "inproc://backend"),

    start_server_workers(Ctx, 5),

    %%  Connect backend to frontend via a queue device
    erlzmq_device:queue(Frontend, Backend),

    ok = erlzmq:term(Ctx).

start_server_workers(_Ctx, 0) -> ok;
start_server_workers(Ctx, N) when N > 0 ->
    spawn(fun() -> server_worker(Ctx) end),
    start_server_workers(Ctx, N - 1).

%%  Accept a request and reply with the same text a random number of
%%  times, with random delays between replies.
%%
server_worker(Ctx) ->
    random:seed(now()),
    {ok, Worker} = erlzmq:socket(Ctx, dealer),
    ok = erlzmq:connect(Worker, "inproc://backend"),
    server_worker_loop(Worker).

server_worker_loop(Worker) ->
    {ok, Address} = erlzmq:recv(Worker),
    {ok, Content} = erlzmq:recv(Worker),
    send_replies(Worker, Address, Content, random:uniform(4) - 1),
    server_worker_loop(Worker).

send_replies(_, _, _, 0) -> ok;
send_replies(Worker, Address, Content, N) when N > 0 ->
    %%  Sleep for some fraction of a second
    timer:sleep(random:uniform(1000)),
    ok = erlzmq:send(Worker, Address, [sndmore]),
    ok = erlzmq:send(Worker, Content),
    send_replies(Worker, Address, Content, N - 1).

%%  This main thread simply starts several clients, and a server, and then
%%  waits for the server to finish.
%%
main(_) ->
    spawn(fun() -> client_task() end),
    spawn(fun() -> client_task() end),
    spawn(fun() -> client_task() end),
    spawn(fun() -> server_task() end),
    timer:sleep(5000).
