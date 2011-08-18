#! /usr/bin/env escript
%%
%% Simple request-reply broker
%%

main(_) ->
    %% Prepare our context and sockets
    {ok, Context} = erlzmq:context(),
    {ok, Frontend} = erlzmq:socket(Context, [router, {active, true}]),
    {ok, Backend} = erlzmq:socket(Context, [dealer, {active, true}]),
    ok = erlzmq:bind(Frontend, "tcp://*:5559"),
    ok = erlzmq:bind(Backend, "tcp://*:5560"),

    %% Switch messages between sockets
    loop(Frontend, Backend),

    %% We never get here but clean up anyhow
    ok = erlzmq:close(Frontend),
    ok = erlzmq:close(Backend),
    ok = erlzmq:term(Context).

loop(Frontend, Backend) ->
    receive
        {zmq, Frontend, Msg, Flags} ->
            case proplists:get_bool(rcvmore, Flags) of
                true ->
                    erlzmq:send(Backend, Msg, [sndmore]);
                false ->
                    erlzmq:send(Backend, Msg)
            end;
        {zmq, Backend, Msg, Flags} ->
            case proplists:get_bool(rcvmore, Flags) of
                true ->
                    erlzmq:send(Frontend, Msg, [sndmore]);
                false ->
                    erlzmq:send(Frontend, Msg)
            end
    end,
    loop(Frontend, Backend).
