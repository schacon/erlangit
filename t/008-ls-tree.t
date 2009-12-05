#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl -boot start_sasl -noshell

main(_) ->
  Git = git:open("test_git"),
  {ok, Tree} = git:object(Git, "c67da89afe12df86e7b8324f1ac5fa470de2bb48"),
  git_io:print_tree(Tree).

