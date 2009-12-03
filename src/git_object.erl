%%
%% Git Object Parsers
%%

%% TODO: replace regexp:first_match with re

-module(git_object).
-export([parse_commit/2]).

-include("git.hrl").

parse_commit(Sha, Data) ->
  CommitString = binary_to_list(Data),
  {match, Offset, Len} = regexp:first_match(CommitString, "\n\n"),
  {Meta, Message} = lists:split(Offset + Len - 1, CommitString),
  Parents   = parse_commit_parents(Meta),
  Tree      = extract_one(Meta, "tree (.*)"),
  Author    = extract_one(Meta, "author (.*)"),
  Committer = extract_one(Meta, "committer (.*)"),
  Encoding  = extract_one(Meta, "encoding (.*)"),
  %io:format("Parents:~p~nTree:~p~nAuthor:~p~nMessage:~p~n~n", [Parents, Tree, Author, Message]),
  Commit = #commit{sha=Sha, tree=Tree, parents=Parents,
                   author=Author, committer=Committer,
                   encoding=Encoding, message=Message},
  {ok, Commit}.

parse_commit_parents(Data) ->
  Parents = extract_multi(Data, "parent (.*)"),
  extract_matches(Parents).

extract_matches([Match|Rest]) ->
  [_Full, Data] = Match,
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
      [_Full, Value] = Captured,
      Value;
    _Else ->
      ""
  end.
