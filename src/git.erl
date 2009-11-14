%%
%% Partial Git Implementation
%%

-module(git).
-export([open/1, read_object/2]).

%%-define(cassandra_ZERO, 0).

%-record(git_dir, {path}).
%-record(commit, {commit_sha, tree_sha, parents, author, committer, encoding, message}).

% Cp = #commit{sha=Sha, tree=Tree},

open(Path) ->
  % normalize the path (look for .git, etc)
  {Path}.

references(Git) ->
  % read all the refs from disk/packed-refs and return an array
  {Git}.

print_branches(Git) ->
  % print branches out to stdout
  io:fwrite("Branches:~n").

print_log(Git, Ref) ->
  % traverse the reference, printing out all the log information to stdout
  io:fwrite("Log:~n").

read_object(Git, ObjectSha) ->
  RawData = get_object_data(Git, ObjectSha),
  {Type, Size, Data} = extract_object_data(RawData),
  % de-zlib the data
  % return object data
  {Type, Size, Data}.

%% TODO: make this more efficient - this is ridiculous
%%       should be able to do this as a binary
extract_object_data(CompData) ->
  RawData = binary_to_list(zlib:uncompress(CompData)),
  Split = string:chr(RawData, 0),
  {Header, Data} = lists:split(Split, RawData),
  Split2 = string:chr(Header, 32),
  Header2 = lists:sublist(Header, length(Header) - 1),
  {Type, Size} = lists:split(Split2, Header2),
  Type2 = lists:sublist(Type, length(Type) - 1),
  {binary_to_atom(list_to_binary(Type2), latin1), list_to_integer(Size), list_to_binary(Data)}.

git_dir(Git) ->
  {Path} = Git,
  Path.

% get the raw object data out of loose or packed formats
get_object_data(Git, ObjectSha) ->
  % see if the object is loose, read the data
  % else check the packfile indexes and get the object out of a packfile
  First = string:substr(ObjectSha, 1, 2),
  Second = string:substr(ObjectSha, 3, 38),
  FileName = git_dir(Git) ++ "/objects/" ++ First ++ "/" ++ Second,
  case file:read_file(FileName) of
    {ok, Data} ->
      Data;
    _Else ->
      invalid
  end.