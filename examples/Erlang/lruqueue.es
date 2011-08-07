#! /usr/bin/env escript
%%
%% Least-recently used (LRU) queue device
%% Clients and workers are shown here in-process
%%
%% While this example runs in a single process, that is just to make
%% it easier to start and stop the example. Each thread has its own
%% context and conceptually acts as a separate process.
%%

-define(NBR_CLIENTS, 10).
-define(NBR_WORKERS, 3).

%% Basic request-reply client using REQ socket
%% Since s_send and s_recv can't handle 0MQ binary identities we
%% set a printable text identity to allow routing.
%%
client_task() ->
    {ok, Context} = erlzmq:context(),
    {ok, Client} = erlzmq:socket(Context, req),
    ok = erlzmq:setsockopt(Client, identity, pid_to_list(self())),
    ok = erlzmq:connect(Client, "ipc://frontend.ipc"),

    %% Send request, get reply
    ok = erlzmq:send(Client, <<"HELLO">>),
    {ok, Reply} = erlzmq:recv(Client),
    io:format("Client: ~s~n", [Reply]),

    ok = erlzmq:close(Client),
    ok = erlzmq:term(Context).

%% Worker using REQ socket to do LRU routing
%% Since s_send and s_recv can't handle 0MQ binary identities we
%% set a printable text identity to allow routing.
%%
worker_task() ->
    {ok, Context} = erlzmq:context(),
    {ok, Worker} = erlzmq:socket(Context, req),
    ok = erlzmq:setsockopt(Worker, identity, pid_to_list(self())),
    ok = erlzmq:connect(Worker, "ipc://backend.ipc"),

    %% Tell broker we're ready for work
    ok = erlzmq:send(Worker, <<"READY">>),

    worker_loop(Worker),

    ok = erlzmq:close(Worker),
    ok = erlzmq:term(Context).

worker_loop(Worker) ->
    %% Read and save all frames until we get an empty frame
    %% In this example there is only 1 but it could be more
    {ok, Address} = erlzmq:recv(Worker),
    {ok, <<>>} = erlzmq:recv(Worker),

    %% Get request, send reply
    {ok, Request} = erlzmq:recv(Worker),
    io:format("Worker: ~s~n", [Request]),

    ok = erlzmq:send(Worker, Address, [sndmore]),
    ok = erlzmq:send(Worker, <<>>, [sndmore]),
    ok = erlzmq:send(Worker, <<"OK">>),

    worker_loop(Worker).

main(_) ->
    %% Prepare our context and sockets
    {ok, Context} = erlzmq:context(),
    {ok, Frontend} = erlzmq:socket(Context, [router, {active, true}]),
    {ok, Backend} = erlzmq:socket(Context, [router, {active, true}]),
    ok = erlzmq:bind(Frontend, "ipc://frontend.ipc"),
    ok = erlzmq:bind(Backend, "ipc://backend.ipc"),

    start_clients(?NBR_CLIENTS),
    start_workers(?NBR_WORKERS),

    %% Logic of LRU loop
    %% - Poll backend always, frontend only if 1+ worker ready
    %% - If worker replies, queue worker as ready and forward reply
    %%   to client if necessary
    %% - If client requests, pop next worker and send request to it

    %% Queue of available workers
    WorkerQueue = queue:new(),

    lru_loop(?NBR_CLIENTS, WorkerQueue, Frontend, Backend),

    ok = erlzmq:close(Frontend),
    ok = erlzmq:close(Backend),
    ok = erlzmq:term(Context).

start_clients(0) -> ok;
start_clients(N) when N > 0 ->
    spawn(fun() -> client_task() end),
    start_clients(N - 1).

start_workers(0) -> ok;
start_workers(N) when N > 0 ->
    spawn(fun() -> worker_task() end),
    start_workers(N - 1).

lru_loop(0, _, _, _) -> ok;
lru_loop(NumClients, WorkerQueue, Frontend, Backend) when NumClients > 0 ->
    case queue:len(WorkerQueue) of
        0 ->
            receive
                {zmq, Backend, Msg, _} ->
                    lru_loop_backend(
                      NumClients, WorkerQueue, Frontend, Backend, Msg)
            end;
        _ ->
            receive
                {zmq, Backend, Msg, _} ->
                    lru_loop_backend(
                      NumClients, WorkerQueue, Frontend, Backend, Msg);
                {zmq, Frontend, Msg, _} ->
                    lru_loop_frontend(
                      NumClients, WorkerQueue, Frontend, Backend, Msg)
            end
    end.

lru_loop_backend(NumClients, WorkerQueue, Frontend, Backend, WorkerAddr) ->
    %% Queue worker address for LRU routing
    NewWorkerQueue = queue:in(WorkerAddr, WorkerQueue),
    {ok, <<>>} = active_recv(Backend),
    case active_recv(Backend) of
        {ok, <<"READY">>} ->
            lru_loop(NumClients, NewWorkerQueue, Frontend, Backend);
        {ok, ClientAddr} ->
            {ok, <<>>} = active_recv(Backend),
            {ok, Reply} = active_recv(Backend),
            erlzmq:send(Frontend, ClientAddr, [sndmore]),
            erlzmq:send(Frontend, <<>>, [sndmore]),
            erlzmq:send(Frontend, Reply),
            lru_loop(NumClients - 1, NewWorkerQueue, Frontend, Backend)
    end.

lru_loop_frontend(NumClients, WorkerQueue, Frontend, Backend, ClientAddr) ->
    %% Get next client request, route to LRU worker
    %% Client request is [address][empty][request]
    {ok, <<>>} = active_recv(Frontend),
    {ok, Request} = active_recv(Frontend),

    {{value, WorkerAddr}, NewWorkerQueue} = queue:out(WorkerQueue),
    ok = erlzmq:send(Backend, WorkerAddr, [sndmore]),
    ok = erlzmq:send(Backend, <<>>, [sndmore]),
    ok = erlzmq:send(Backend, ClientAddr, [sndmore]),
    ok = erlzmq:send(Backend, <<>>, [sndmore]),
    ok = erlzmq:send(Backend, Request),

    lru_loop(NumClients, NewWorkerQueue, Frontend, Backend).

active_recv(Socket) ->
    receive
        {zmq, Socket, Msg, _Flags} -> {ok, Msg}
    end.
