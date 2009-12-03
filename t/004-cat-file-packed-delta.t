#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
    Git = git:open("test_git"),
    {Type, Size, Data} = git:read_object(Git, "9cad8d7e8ee5b3b6fcb401ac9dcc557dd808762d"), %% tree
    io:fwrite("Data: ~p ~p~n", [Type, Size]),    
    io:fwrite("Data: ~p~n", [Data]),
    io:fwrite("Data: ~s~n", [binary_to_list(Data)]).