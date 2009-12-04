#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
    Git = git:open("test_git"),
    {Type, Size, Data} = git:object_data(Git, "ff69c3684a18592c741332b290492aa39d980e02"), %% blob
    io:fwrite("Data: ~p ~p~n", [Type, Size]),
    io:fwrite("Data: ~p~n", [Data]),
    io:fwrite("Data: ~s~n", [binary_to_list(Data)]).

