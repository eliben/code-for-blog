The ``index-doc.go`` program should be pointed to the Go source tree, with flags
depending on use. To use in command-line completion:

  go run index-doc.go -dumpraw /usr/local/go/src/ > /tmp/gosyms
