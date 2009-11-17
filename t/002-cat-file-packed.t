#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
    Git = git:open("test_git"),
    {Type, Size, Data} = git:read_object(Git, "aa7dfe7c2a634cb9e7a9d5838eb58fe150ebd7fb"), %% tree
    io:fwrite("Data: ~p ~p~n", [Type, Size]),    
    io:fwrite("Data: ~p~n", [Data]).