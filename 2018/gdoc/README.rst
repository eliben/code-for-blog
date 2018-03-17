The ``index-doc.go`` program should be pointed to the Go source tree, with flags
depending on use. To use in command-line completion:

  go run index-doc.go -dumpraw /usr/lib/go-1.9/src/ > /tmp/gosyms

And then use with dmenu, e.g.:

  cat /tmp/gosyms | dmenu -l 20 -i -b -p "go doc" | xargs go doc

For "web" use, run the indexer with:

  go run index-doc.go -dumpjs /usr/lib/go-1.9/src/ > all_symbols.js

And then open ``gdoc.html`` in the browser.
