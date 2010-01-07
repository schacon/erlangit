-include_lib("eunit/include/eunit.hrl").

object_data_blob_test() ->
  Git = git:open("test_git"),
  {blob, 13, <<"test\nanother\n">>} = git:object_data(Git, "8d47f3435ce5dfd0b2ab5758590c2db21b5294b4").

object_data_tree_test() ->
  Git = git:open("test_git"),
  Data = <<49,48,48,54,52,52,32,82,69,65,68,77,69,0,157,174,175,185,134,76,244,48,
           85,174,147,190,176,175,214,199,209,68,191,164,52,48,48,48,48,32,100,
           105,114,111,110,101,0,128,43,239,80,141,163,41,195,101,202,199,105,170,
           6,7,56,74,5,166,173,52,48,48,48,48,32,100,105,114,116,119,111,0,1,50,
           186,7,196,230,60,172,232,178,133,56,3,228,235,228,20,151,111,253>>,
  {tree, 100, Data} = git:object_data(Git, "aa7dfe7c2a634cb9e7a9d5838eb58fe150ebd7fb").

object_exists_test() ->
  Git = git:open("test_git"),
  true = git:object_exists(Git, "8d47f3435ce5dfd0b2ab5758590c2db21b5294b4"), %% LooseT
  false = git:object_exists(Git, "ad47f3435ce5afd0b2ab5758590c2db21b5294b4"), %% LooseF
  true = git:object_exists(Git, "be62addb149d286893e2ec254e0dc783a871e8af"), %% PackT
  false = git:object_exists(Git, "ae62addb149d286893e2ec254e0dc783a871e8af"). %% PackF

cat_file_packed_delta_test() ->
  Git = git:open("test_git"),
  Data = <<49,48,48,54,52,52,32,82,69,65,68,77,69,0,157,174,175,185,134,76,244,48,
           85,174,147,190,176,175,214,199,209,68,191,164,52,48,48,48,48,32,100,
           105,114,111,110,101,0,128,43,239,80,141,163,41,195,101,202,199,105,170,
           6,7,56,74,5,166,173>>,
  {tree, 67, Data} = git:object_data(Git, "9cad8d7e8ee5b3b6fcb401ac9dcc557dd808762d").

cat_file_packed_ref_delta_test() ->
  Git = git:open("test_git"),
  Data = <<"# -*- ruby -*-\n\nrequire 'rubygems'\nrequire 'hoe'\nrequire './lib/grit.rb'\n\nHoe.new('grit', Grit::VERSION) do |p|\n  p.rubyforge_name = 'grit'\n  # p.author = 'FIX'\n  # p.email = 'FIX'\n  # p.summary = 'FIX'\n  # p.description = p.paragraphs_of('README.txt', 2..5).join(\"\\n\\n\")\n  # p.url = p.paragraphs_of('README.txt', 0).first.split(/\\n/)[1..-1]\n  p.changes = p.paragraphs_of('History.txt', 0..1).join(\"\\n\\n\")\nend\n\n# vim: syntax=Ruby\n">>,
  {blob, 430, Data} = git:object_data(Git, "ff69c3684a18592c741332b290492aa39d980e02").

rev_list_test() ->
  Git = git:open("test_git"),
  RevList = ["25daa907ccb6feb267bfec70a130d5fe13e48a79",
             "dd991a4966e8807d448305e67a9b3727efc6060c",
             "be62addb149d286893e2ec254e0dc783a871e8af",
             "208fc4a6a08fb1e5136cf9943d79ad81097a0f36",
             "ef8285c69ceac3f2ff2fee154d3d6ad6a71fb826",
             "61fa970afbf82f79611220203213cd9562a809e3",
             "a361a3163a1b521f277e3877232d94b787750c36"],
  RevList = git:rev_list(Git, ["25daa907ccb6feb267bfec70a130d5fe13e48a79"]).
