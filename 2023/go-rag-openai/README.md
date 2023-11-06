Exploring the chunks DB from the command-line:

    echo ".tables" | sqlite3 chunks.db
    echo "select id, path, nchunk, length(content) from chunks" | sqlite3 chunks.db
