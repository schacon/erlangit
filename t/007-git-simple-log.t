#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
    Git = git:open("test_git"),
    git_io:print_log(Git, ["25daa907ccb6feb267bfec70a130d5fe13e48a79"]).

