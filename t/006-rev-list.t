#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
    Git = git:open("test_git"),
    RevList = git:rev_list(Git, ["25daa907ccb6feb267bfec70a130d5fe13e48a79"]), %% blob
    io:fwrite("RevList: ~p~n", [RevList]).

