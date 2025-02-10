Running dot benchmarks from the command line:

    $ python3 -m timeit -s "import dot; a = [1]*1000000; b = [2]*1000000" "dot.dotProductLoop(a, b)"
    10 loops, best of 5: 20.7 msec per loop
    $ python3 -m timeit -s "import dot; a = [1]*1000000; b = [2]*1000000" "dot.dotProductZip(a, b)"
    20 loops, best of 5: 16.3 msec per loop
    $ python3 -m timeit -s "import dot; a = [1]*1000000; b = [2]*1000000" "dot.dotProductStarmap(a, b)"
    20 loops, best of 5: 10.5 msec per loop

TODO Features needed:

1) Maximum detection of loop count N
2) Finding "best of K" (5 by default in both Go and Python)
3) Divide total exec by N to get "per loop" time