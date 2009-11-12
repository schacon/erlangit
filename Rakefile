require 'rubygems'
require 'rake'

ERLC_FLAGS = "+debug_info -W2 -o ../ebin"

task :default do
  cd "src"
  sh "erlc #{ERLC_FLAGS} *.erl"
end
