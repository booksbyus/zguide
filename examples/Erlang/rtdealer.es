#! /usr/bin/env escript
%%
%% Custom routing Router to Dealer
%%
%% While this example runs in a single process, that is just to make
%% it easier to start and stop the example. Each thread has its own
%% context and conceptually acts as a separate process.
%%

%% We start multiple workers in this process - these would normally be on
%% different nodes...

worker_task(Id) ->
    {ok, Context} = erlzmq:context(),
    {ok, Worker} = erlzmq:socket(Context, dealer),
    ok = erlzmq:setsockopt(Worker, identity, Id),
    ok = erlzmq:connect(Worker, "ipc://routing.ipc"),

    Count = count_messages(Worker, 0),
    io:format("~s received: ~b~n", [Id, Count]),

    ok = erlzmq:close(Worker),
    ok = erlzmq:term(Context).

count_messages(Socket, Count) ->
    case erlzmq:recv(Socket) of
        {ok, <<"END">>} -> Count;
        {ok, _} -> count_messages(Socket, Count + 1)
    end.

main(_) ->
    {ok, Context} = erlzmq:context(),
    {ok, Client} = erlzmq:socket(Context, router),
    ok = erlzmq:bind(Client, "ipc://routing.ipc"),

    spawn(fun() -> worker_task(<<"A">>) end),
    spawn(fun() -> worker_task(<<"B">>) end),

    %% Wait for threads to connect, since otherwise the messages
    %% we send won't be routable.
    timer:sleep(1000),

    %% Send 10 tasks scattered to A twice as often as B
    lists:foreach(
      fun(Num) ->
              %% Send two message parts, first the address
              case Num rem 3 of
                  0 ->
                      ok = erlzmq:send(Client, <<"B">>, [sndmore]);
                  _ ->
                      ok = erlzmq:send(Client, <<"A">>, [sndmore])
              end,
              %% And then the workload
              ok = erlzmq:send(Client, <<"Workload">>)
      end, lists:seq(1, 10)),

    ok = erlzmq:send(Client, <<"A">>, [sndmore]),
    ok = erlzmq:send(Client, <<"END">>),

    ok = erlzmq:send(Client, <<"B">>, [sndmore]),
    ok = erlzmq:send(Client, <<"END">>),

    %% Workers use separate context, so we can't rely on Context term
    %% below to wait for them to finish. Manually wait instead.
    timer:sleep(100),

    ok = erlzmq:close(Client),
    ok = erlzmq:term(Context).
