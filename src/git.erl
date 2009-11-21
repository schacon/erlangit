%%
%% Partial Git Implementation
%%

-module(git).
-export([open/1, read_object/2]).

-include("packindex.hrl").

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

git_dir(Git) ->
  {Path} = Git,
  Path.

% get the raw object data out of loose or packed formats
read_object(Git, ObjectSha) ->
  % see if the object is loose, read the data
  % else check the packfile indexes and get the object out of a packfile
  First = string:substr(ObjectSha, 1, 2),
  Second = string:substr(ObjectSha, 3, 38),
  FileName = git_dir(Git) ++ "/objects/" ++ First ++ "/" ++ Second,
  case file:read_file(FileName) of
    {ok, Data} ->
      extract_loose_object_data(Data);
    _Else ->
      get_packfile_object_data(Git, ObjectSha)
  end.

%% TODO: make this more efficient - this is ridiculous
%%       should be able to do this as a binary
extract_loose_object_data(CompData) ->
  RawData = binary_to_list(zlib:uncompress(CompData)),
  Split = string:chr(RawData, 0),
  {Header, Data} = lists:split(Split, RawData),
  Split2 = string:chr(Header, 32),
  Header2 = lists:sublist(Header, length(Header) - 1),
  {Type, Size} = lists:split(Split2, Header2),
  Type2 = lists:sublist(Type, length(Type) - 1),
  {binary_to_atom(list_to_binary(Type2), latin1), list_to_integer(Size), list_to_binary(Data)}.

get_packfile_object_data(Git, ObjectSha) ->
  io:fwrite("SHA:~p~n", [ObjectSha]),
  PackIndex = git_dir(Git) ++ "/objects/pack",
  case file:list_dir(PackIndex) of
    {ok, Filenames} ->
      Indexes = lists:filter(fun(X) -> string_ends_with(X, ".idx") end, Filenames),
      case get_packfile_with_object(Git, Indexes, ObjectSha) of
        {ok, Packfile, Offset} ->
          PackFilePath = git_dir(Git) ++ "/objects/pack/" ++ Packfile,
          packfile:get_packfile_data(PackFilePath, Offset);
        _Else ->
          invalid
      end;
    _Else ->
      invalid
  end.

get_packfile_with_object(Git, [Index|Rest], ObjectSha) ->
  PackIndex = git_dir(Git) ++ "/objects/pack/" ++ Index,
  io:fwrite("Looking for ~p in ~p~n", [ObjectSha, PackIndex]),
  case file:read_file(PackIndex) of
    {ok, Data} ->
      case packindex:extract_packfile_index(Data) of
        {ok, IndexData} ->
          io:fwrite("PackIndex Size:~p~n", [IndexData#index.size]),
          io:fwrite("IndexData:~p~n", [IndexData]),
          case packindex:object_offset(IndexData, ObjectSha) of
            {ok, Offset} ->
              Packfile = replace_string_ending(Index, ".idx", ".pack"),
              {ok, Packfile, Offset};
            not_found ->
              get_packfile_with_object(Git, Rest, ObjectSha)
          end;
        Else ->
          io:fwrite("Invalid, Biatch~p~n", [Else]),
          invalid
      end;
    _Else ->
      invalid
  end;
get_packfile_with_object(Git, [], ObjectSha) ->
  not_found.

replace_string_ending(String, Ending, NewEnding) ->
  Base = string:substr(String, 1, length(String) - length(Ending)),
  Base ++ NewEnding.

string_ends_with(File, Ending) ->  
  FileEnding = string:substr(File, length(File) - length(Ending) + 1, length(Ending)),
  FileEnding =:= Ending.







