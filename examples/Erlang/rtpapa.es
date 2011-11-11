#! /usr/bin/env escript
%%
%% Custom routing Router to Papa (ROUTER to REP)
%%

%% We will do this all in one thread to emphasize the sequence
%% of events…
main(_) ->
    {ok, Context} = erlzmq:context(),

    {ok, Client} = erlzmq:socket(Context, router),
    ok = erlzmq:bind(Client, "ipc://routing.ipc"),

    {ok, Worker} = erlzmq:socket(Context, rep),
    ok = erlzmq:setsockopt(Worker, identity, <<"A">>),
    ok = erlzmq:connect(Worker, "ipc://routing.ipc"),

    %% Wait for the worker to connect so that when we send a message
    %% with routing envelope, it will actually match the worker...
    timer:sleep(1000),

    %% Send papa address, address stack, empty part, and request
    erlzmq:send(Client, <<"A">>, [sndmore]),
    erlzmq:send(Client, <<"address 3">>, [sndmore]),
    erlzmq:send(Client, <<"address 2">>, [sndmore]),
    erlzmq:send(Client, <<"address 1">>, [sndmore]),
    erlzmq:send(Client, <<>>, [sndmore]),
    erlzmq:send(Client, <<"This is the workload">>),

    %% Worker should get just the workload
    erlzmq_util:dump(Worker),

    %% We don't play with envelopes in the Worker
    erlzmq:send(Worker, <<"This is the reply">>),

    %% Now dump what we got off the ROUTER socket...
    erlzmq_util:dump(Client),

    erlzmq:close(Client),
    erlzmq:close(Worker),
    erlzmq:term(Context).
