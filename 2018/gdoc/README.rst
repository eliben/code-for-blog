The ``index-doc.go`` program should be pointed to the Go source tree, with flags
depending on use. To use in command-line completion:

    go run index-doc.go -dumpraw /usr/local/go/src/ > $HOME/.gosyms

And then use with fzf, e.g.:

    go doc `fzf < $HOME/.gosyms`

For "web" use, run the indexer with:

    go run index-doc.go -dumpjs /usr/local/go/src/ > all_symbols.js

And then open ``gdoc.html`` in the browser.
