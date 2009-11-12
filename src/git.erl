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
  %% normalize the path (look for .git, etc)
  {Path}.

read_object(Git, ObjectSha) ->
  %% find object path (loose)
  %% read object data
  %% de-header it
  %% de-zlib it
  %% return object data
  Type = commit,
  Size = 355,
  Data = <<"hello">>,
  {Type, Size, Data}.
