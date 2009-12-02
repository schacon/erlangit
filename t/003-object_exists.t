#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
    Git = git:open("test_git"),
    Result = git:object_exists(Git, "8d47f3435ce5dfd0b2ab5758590c2db21b5294b4"), %% blob
    io:fwrite("LooseT: ~p ~n", [Result]),
    Result2 = git:object_exists(Git, "ad47f3435ce5afd0b2ab5758590c2db21b5294b4"), %% blob
    io:fwrite("LooseF: ~p ~n", [Result2]),
    Result3 = git:object_exists(Git, "be62addb149d286893e2ec254e0dc783a871e8af"), %% blob
    io:fwrite("PackT : ~p ~n", [Result3]),
    Result4 = git:object_exists(Git, "ae62addb149d286893e2ec254e0dc783a871e8af"), %% blob
    io:fwrite("PackF : ~p ~n", [Result4]).
    
