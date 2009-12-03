%%
%% Partial Git Implementation
%%

-module(git_object).
-export([parse_commit/1, get_parents/1]).

parse_commit(Data) ->
  io:format("Commit: ~p~n", [Data]),
  Data.

get_parents(Commit) ->
  [].

