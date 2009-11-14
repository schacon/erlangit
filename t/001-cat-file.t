#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
    Git = git:open("/Users/schacon/projects/subsucka/.git"),
    {Type, Size, Data} = git:read_object(Git, "019b03b2b5f184a9bbec13bfb4be53b80561f213"), %% blob
    io:fwrite("Data: ~p ~p~n", [Type, Size]),    
    io:fwrite("Data: ~p~n", [Data]).