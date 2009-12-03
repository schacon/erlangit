
-record(index, {header,
                version,
                size,
                fanout,
                shalist,
                crclist,
                offsets,
                packcs}).

-record(commit, {parents,
                 tree,
                 author,
                 committer,
                 encoding,
                 message}).
