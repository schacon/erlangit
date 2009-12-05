#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
    Git = git:open("test_git"),
    {Time, Value} = timer:tc(git, object_data, [Git, "be62addb149d286893e2ec254e0dc783a871e8af"]),
    io:fwrite("Data: ~p:~p~n", [Time, Value]).

