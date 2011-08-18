#! /usr/bin/env escript
%%
%% Task worker - design 2
%% Adds pub-sub flow to receive and respond to kill signal
%%

main(_) ->
    {ok, Context} = erlzmq:context(),

    %% Socket to receive messages on
    {ok, Receiver} = erlzmq:socket(Context, [pull, {active, true}]),
    ok = erlzmq:connect(Receiver, "tcp://localhost:5557"),

    %% Socket to send messages to
    {ok, Sender} = erlzmq:socket(Context, push),
    ok = erlzmq:connect(Sender, "tcp://localhost:5558"),

    %% Socket for control input
    {ok, Controller} = erlzmq:socket(Context, [sub, {active, true}]),
    ok = erlzmq:connect(Controller, "tcp://localhost:5559"),
    ok = erlzmq:setsockopt(Controller, subscribe, <<>>),

    %% Process messages from receiver and controller
    process_messages(Receiver, Controller, Sender),

    %% Finished
    ok = erlzmq:close(Receiver),
    ok = erlzmq:close(Sender),
    ok = erlzmq:close(Controller),
    ok = erlzmq:term(Context).

process_messages(Receiver, Controller, Sender) ->
    receive
        {zmq, Receiver, Msg, _Flags} ->
            %% Do the work
            timer:sleep(list_to_integer(binary_to_list(Msg))),

            %% Send results to sink
            ok = erlzmq:send(Sender, Msg),

            %% Simple progress indicator for the viewer
            io:format("."),

            process_messages(Receiver, Controller, Sender);

        {zmq, Controller, _Msg, _Flags} ->
            %% Any waiting controller command acts as 'KILL'
            ok
    end.
