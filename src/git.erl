%% -*- erlang-indent-level: 2 -*-
%%
%% Partial Git Implementation
%%

-module(git).
-export([open/1, object_data/2, object/2, object_exists/2, rev_list/2]).

-ifdef(TEST).
-include("etest/git_test.erl").
-endif.

-include("git.hrl").

%%-define(cassandra_ZERO, 0).

open(Path) ->
  % normalize the path (look for .git, etc)
  #git{path = Path}.

%references(Git) ->
  % read all the refs from disk/packed-refs and return an array
  %{Git}.

rev_list(Git, Shas) ->
  Graph = digraph:new(),
  rev_list(Git, Graph, Shas),
  digraph_utils:topsort(Graph).

rev_list(Git, Graph, [Sha|Shas]) ->
  {ok, Commit} = object(Git, Sha),
  digraph:add_vertex(Graph, Sha),
  Parents = Commit#commit.parents,
  AddParents = rev_list_add_edges(Graph, Sha, Parents),
  rev_list(Git, Graph, AddParents ++ Shas);
rev_list(_Git, _Graph, []) ->
  ok.

rev_list_add_edges(Graph, Sha, [Parent|Rest]) ->
  Vertex = case digraph:vertex(Graph, Parent) of
    false ->
      digraph:add_vertex(Graph, Parent);
    _Else ->
      []
  end,
  digraph:add_edge(Graph, Sha, Parent),
  [Vertex|rev_list_add_edges(Graph, Sha, Rest)];
rev_list_add_edges(_Graph, _Commit, []) ->
  [].

object(Git, Sha) ->
  {Type, _Size, Data} = object_data(Git, Sha),
  git_object:parse_object(Sha, Data, Type).

git_dir(Git) ->
  Git#git.path.

object_exists(Git, ObjectSha) ->
  LoosePath = get_loose_object_path(Git, ObjectSha),
  case filelib:is_file(LoosePath) of
    true ->
      true;
    false ->
      case find_packfile_with_object(Git, ObjectSha) of
        {ok, _PackFilePath, _Offset} ->
          true;
        _Else ->
          false
      end
  end.

% get the raw object data out of loose or packed formats
% see if the object is loose, read the data
% else check the packfile indexes and get the object out of a packfile
% TODO: cache calls to this (at least for commit/tree objects)
object_data(Git, ObjectSha) ->
  LoosePath = get_loose_object_path(Git, ObjectSha),
  case file:read_file(LoosePath) of
    {ok, Data} ->
      extract_loose_object_data(Data);
    _Else ->
      get_packfile_object_data(Git, ObjectSha)
  end.

get_loose_object_path(Git, ObjectSha) ->
  First = string:substr(ObjectSha, 1, 2),
  Second = string:substr(ObjectSha, 3, 38),
  git_dir(Git) ++ "/objects/" ++ First ++ "/" ++ Second.

extract_loose_object_data(CompData) ->
  extract_loose_object_1(zlib:uncompress(CompData), []).

extract_loose_object_1(<<$\s,T/binary>>, Type0) ->
  Type = list_to_atom(lists:reverse(Type0)),
  extract_loose_object_2(T, Type, 0);
extract_loose_object_1(<<C,T/binary>>, Type) ->
  extract_loose_object_1(T, [C|Type]).

extract_loose_object_2(<<0,Data/binary>>, Type, Sz) ->
  {Type, Sz, Data};
extract_loose_object_2(<<Digit,T/binary>>, Type, Sz) ->
  extract_loose_object_2(T, Type, Sz*10+Digit-$0).

get_packfile_object_data(Git, ObjectSha) ->
  case find_packfile_with_object(Git, ObjectSha) of
    {ok, PackFilePath, Offset} ->
      packfile:get_packfile_data(Git, PackFilePath, Offset);
    _Else ->
      invalid
  end.

find_packfile_with_object(Git, ObjectSha) ->
  %io:fwrite("SHA:~p~n", [ObjectSha]),
  PackIndex = git_dir(Git) ++ "/objects/pack",
  case file:list_dir(PackIndex) of
    {ok, Filenames} ->
      Indexes = lists:filter(fun(X) -> string_ends_with(X, ".idx") end, Filenames),
      case get_packfile_with_object(Git, Indexes, ObjectSha) of
        {ok, Packfile, Offset} ->
          PackFilePath = git_dir(Git) ++ "/objects/pack/" ++ Packfile,
          {ok, PackFilePath, Offset};
        _Else ->
          invalid
      end;
    _Else ->
      invalid
  end.

get_packfile_with_object(Git, [Index|Rest], ObjectSha) ->
  PackIndex = git_dir(Git) ++ "/objects/pack/" ++ Index,
  %io:fwrite("Looking for ~p in ~p~n", [ObjectSha, PackIndex]),
  case file:read_file(PackIndex) of
    {ok, Data} ->
      case packindex:extract_packfile_index(Data) of
        {ok, IndexData} ->
          %io:fwrite("PackIndex Size:~p~n", [IndexData#index.size]),
          %io:fwrite("IndexData:~p~n", [IndexData]),
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
get_packfile_with_object(_Git, [], _ObjectSha) ->
  not_found.

replace_string_ending(String, Ending, NewEnding) ->
  Base = string:substr(String, 1, length(String) - length(Ending)),
  Base ++ NewEnding.

string_ends_with(File, Ending) ->
  FileEnding = string:substr(File, length(File) - length(Ending) + 1, length(Ending)),
  FileEnding =:= Ending.

