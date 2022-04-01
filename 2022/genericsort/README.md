# Generic bubble sort experiment

To build:

  $ go build -o bubble.out

Test with `go test -v`, run benchmarks with `go test -bench=.`

For profiling, run something like:

  $ ./bubble.out -cpuprofile cpui.out -kind strinterface
  $ go tool pprof -list bubbleSortInterface ./bubble.out cpui.out

To dump assembly, run:

  $ go tool objdump ./bubble.out

I also find https://github.com/aclements/objbrowse useful for more interactive
browsing of Go assembly code.
