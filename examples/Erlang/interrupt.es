#! /usr/bin/env escript
%%
%% Illustrates the equivalent in Erlang to signal handling for shutdown
%%
%% Erlang applications don't use system signals for shutdown (they can't
%% without some sort of custom native extension). Instead they rely on an
%% explicit shutdown routine, either per process (as illustrated here) or
%% system wide (e.g. init:stop() and OTP application shutdown).
%%

main(_) ->
    %% Start a process that manages its own ZeroMQ startup and shutdown
    Server = start_server(),

    %% Run for a while
    timer:sleep(5000),

    %% Send the process a shutdown message - this could be triggered any number
    %% of ways (e.g. handling `terminate` in an OTP compliant process)
    Server ! {shutdown, self()},

    %% Wait for notification that the process has exited cleanly
    receive
        {ok, Server} -> ok
    end.

start_server() ->
    %% Start the server in a separate Erlang process
    spawn(
      fun() ->
              %% The process manages its own ZeroMQ context
              {ok, Context} = erlzmq:context(),
              {ok, Socket} = erlzmq:socket(Context, [rep, {active, true}]),
              ok = erlzmq:bind(Socket, "tcp://*:5555"),
              io:format("Server started on port 5555~n"),
              loop(Context, Socket)
      end).

loop(Context, Socket) ->
    receive
        {zmq, Socket, Msg, _Flags} ->
            erlzmq:send(Socket, <<"You said: ", Msg/binary>>),
            timer:sleep(1000),
            loop(Context, Socket);
        {shutdown, From} ->
            io:format("Stopping server... "),
            ok = erlzmq:close(Socket),
            ok = erlzmq:term(Context),
            io:format("done~n"),
            From ! {ok, self()}
    end.
