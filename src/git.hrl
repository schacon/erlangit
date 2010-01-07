-record(git, {path}).

-record(index, {header,
                version,
                size,
                fanout,
                shalist,
                crclist,
                offsets,
                packcs}).

-record(commit, {sha,
                 parents,
                 tree,
                 author,
                 committer,
                 encoding,
                 message}).

-record(tree, {mode, name, sha}).

