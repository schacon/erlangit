#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
    Git = git:open("test_git"),
    {Type, Size, Data} = git:object_data(Git, "8d47f3435ce5dfd0b2ab5758590c2db21b5294b4"), %% blob
    io:fwrite("Data: ~p ~p~n", [Type, Size]),
    io:fwrite("Data: ~p~n", [Data]).

