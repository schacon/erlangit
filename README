ErlanGit
====================

This is a pure-Erlang implementation of a bunch of Git functionality.  

Currently it is capable of reading Git objects out of loose or packfile 
formats and parsing tree and commit objects into more meaningful data
structures.

It is currently slower than a slow-motion replay of a cold-weather molasses
drip-race judged by sloths who just took Nyquil. Slowly.

Basic API
====================

First you initialize a Git object with the path to a Git repository:

    Git = git:open("test_git")

You can get the type, size and data for any object in Git with the
git:object_data call:

  {Type, Size, Data} = git:object_data(Git, "8d47f3435ce5dfd0b2ab5758590c2db21b5294b4")

Type is a term (one of commit, tree, blob, tag), Size is an int of the size of
the object and Data is a binary of the object data.

You can use object_exists to get a true/false back as to if a given object
exists in the database.

  Result = git:object_exists(Git, "8d47f3435ce5dfd0b2ab5758590c2db21b5294b4")

You can get a topographically ordered listing of the commit SHAs reachable 
froma given array of commit SHAs with the git:rev_list command

  RevList = git:rev_list(Git, ["25daa907ccb6feb267bfec70a130d5fe13e48a79"])

That command will return an array of commit SHA strings:

  RevList: ["25daa907ccb6feb267bfec70a130d5fe13e48a79",
            "dd991a4966e8807d448305e67a9b3727efc6060c",
            "be62addb149d286893e2ec254e0dc783a871e8af",
            "208fc4a6a08fb1e5136cf9943d79ad81097a0f36",
            "ef8285c69ceac3f2ff2fee154d3d6ad6a71fb826",
            "61fa970afbf82f79611220203213cd9562a809e3",
            "a361a3163a1b521f277e3877232d94b787750c36"]

You can get formatted data back for tree and commit objects (and blobs, but it
just returns a string of the blob) with the git:object/2 call

    {ok, Commit} = git:object(Git, "25daa907ccb6feb267bfec70a130d5fe13e48a79")

That will return a Commit object who's record looks like this:

  -record(commit, {sha, parents, tree,
                   author, committer, encoding, message}).

So you can then get the array of parent SHAs with Commit#commit.parents, or
the commit message with Commit#commit.message.

If you run git:object/2 with a tree SHA, it will give you back an array of
tree entries, each of which is a tree record:

   -record(tree, {mode, name, sha}).

IO Git API
===================

There are also a few output functions to help look at the data returned.  You
can get the equivalent of the 'git log' output with the git_io:print_log command.

   git_io:print_log(Git, ["25daa907ccb6feb267bfec70a130d5fe13e48a79"])

That will print to stdout something like this:

  commit 61fa970afbf82f79611220203213cd9562a809e3
  Author: Scott Chacon <schacon@gmail.com> 1258278496 +0100

  added test one

  commit a361a3163a1b521f277e3877232d94b787750c36
  Author: Scott Chacon <schacon@gmail.com> 1258278448 +0100

  first commit

