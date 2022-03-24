Some ways of observing what the compiler produces for this code:

    $ go tool compile -S bubble.go

Alternatively, use the objbrowse tool from https://github.com/aclements/objbrowse

First, build the binary with ``go build``.

Then, in the root of a ``objbrowse`` clone, run:

    $ go run ./cmd/objbrowse <path to here>/example.com

... and open the browser link as instructed. It should also be possible to just
install ``objbrowse`` as a tool instead.
