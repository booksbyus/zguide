#! /usr/bin/env escript
%%
%% Report 0MQ version
%%

main(_) ->
    {Maj, Min, Patch} = erlzmq:version(),
    io:format("Current 0MQ version is ~b.~b.~b~n", [Maj, Min, Patch]).
