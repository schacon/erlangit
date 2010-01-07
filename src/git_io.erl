%%
%% Git Output Printing Functions
%%

-module(git_io).
-export([print_log/2, print_tree/1]).

-ifdef(TEST).
-include("etest/git_io_test.erl").
-endif.

-include("git.hrl").

%print_branches(Git) ->
  % print branches out to stdout
  %io:fwrite("Branches:~n").

print_tree([Entry|Rest]) ->
  io:fwrite("~-7.6s", [Entry#tree.mode]),
  io:fwrite("~s ", [Entry#tree.sha]),
  io:fwrite("~s~n", [Entry#tree.name]),
  print_tree(Rest);
print_tree([]) ->
  ok.

print_log(Git, Refs) ->
  % traverse the reference, printing out all the log information to stdout
  Shas = Refs, % TODO - revparse these
  RevList = git:rev_list(Git, Shas),
  print_log_entries(Git, RevList).

print_log_entries(Git, [Sha|Rest]) ->
  {ok, Commit} = git:object(Git, Sha),
  io:fwrite("commit ~s~n", [Commit#commit.sha]),
  io:fwrite("Author: ~s~n", [Commit#commit.author]),
  io:fwrite("~n"),
  io:fwrite("~s~n", [Commit#commit.message]),
  io:fwrite("~n"),
  print_log_entries(Git, Rest);
print_log_entries(_Git, []) ->
  ok.

