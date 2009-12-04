%%
%% Git Object Parsers
%%

%% TODO: replace regexp:first_match with re

-module(git_object).
-export([parse_object/3]).

-include("git.hrl").

parse_object(Sha, Data, blob) ->
  binary_to_list(Data);

parse_object(Sha, Data, tree) ->
  % mode(6) SP Filename \0 SHA(20)
  TreeString = binary_to_list(Data),
  Tree = parse_tree_string(TreeString),
  {ok, Tree};

parse_object(Sha, Data, commit) ->
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

parse_tree_string([]) ->
  [];
parse_tree_string(Tree) ->
  {Mode, Rest} = read_until(Tree, 32),
  {FileName, Rest2} = read_until(Rest, 0),
  {Sha, Rest3} = lists:split(20, Rest2),
  ShaHex = hex:list_to_hexstr(Sha),
  TreeObj = #tree{sha=ShaHex, mode=Mode, name=FileName},
  [TreeObj | parse_tree_string(Rest3)].

read_until(String, Find) ->
  {Front, Back} = lists:splitwith(fun(A) -> A /= Find end, String),
  {Found, Rest} = lists:split(1, Back),
  {Front, Rest}.

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

