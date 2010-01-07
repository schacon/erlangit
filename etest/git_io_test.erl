-include_lib("eunit/include/eunit.hrl").

print_log_test() ->
  Git = git:open("test_git"),
  git_io:print_log(Git, ["25daa907ccb6feb267bfec70a130d5fe13e48a79"]).

ls_tree_test() ->
  Git = git:open("test_git"),
  {ok, Tree} = git:object(Git, "c67da89afe12df86e7b8324f1ac5fa470de2bb48"),
  git_io:print_tree(Tree).
