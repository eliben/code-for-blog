First, run the chunker to populate the initial chunks DB:

    go run ./cmd/chunker --outdb chunks.db --clear --rootdir <go website _content/doc>

Then, run the embeddings calculation to store embeddings into the DB:

    export OPENAI_API_KEY=<key>
    go run ./cmd/rag --calculate --db chunks.db

Caution! This can consume a bunch of OpenAI credits for embeddings. Now we
can answer the question using context:

    go run ./cmd/rag --answer --db chunks.db

Note: this may take several seconds because we're sending a large (> 3000
tokens) prompt to the LLM.

----

Exploring the chunks DB from the command-line:

    echo ".tables" | sqlite3 chunks.db
    echo "select id, path, nchunk, length(content) from chunks" | sqlite3 chunks.db
    echo "select id, length(embedding) from embeddings" | sqlite3 chunks.db
