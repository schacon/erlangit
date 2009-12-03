%%
%% Partial Git Implementation
%%

-module(git_object).
-export([parse_commit/1, get_parents/1]).

parse_commit(Data) ->
  CommitString = binary_to_list(Data),
  {match, Offset, Len} = regexp:first_match(CommitString, "\n\n"),
  {Meta, Message} = lists:split(Offset + Len - 1, CommitString),
  Parents = parse_commit_parents(Meta),
  Tree = extract_one(Meta, "tree (.*)"),
  Author = extract_one(Meta, "author (.*)"),
  Committer = extract_one(Meta, "committer (.*)"),
  Encoding = extract_one(Meta, "encoding (.*)"),
  %io:format("Parents:~p~nTree:~p~nAuthor:~p~nMessage:~p~n~n", [Parents, Tree, Author, Message]),
  {Tree, Parents, Author, Committer, Encoding, Message}.

parse_commit_parents(Data) ->
  Parents = extract_multi(Data, "parent (.*)"),
  extract_matches(Parents).

extract_matches([Match|Rest]) ->
  [Full, Data] = Match,
  [Data|extract_matches(Rest)];
extract_matches([]) ->
  [].

extract_multi(Data, Regex) ->
  case re:run(Data, Regex, [global, {capture, all, list}]) of
    {match, Captured} ->
      Captured;
    _Else ->
      ""
  end.

extract_one(Data, Regex) ->
  case re:run(Data, Regex, [{capture, all, list}]) of
    {match, Captured} ->
      [Full, Value] = Captured,
      Value;
    _Else ->
      ""
  end.

get_parents(Commit) ->
  {Tree, Parents, Author, Committer, Encoding, Message} = Commit,
  Parents.

