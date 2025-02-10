import dot
import timeit

a = [1] * 1000000
b = [2] * 1000000

# Basic use of timeit
# Have to specify the number of iterations explicitly. Returns total runtime
# for this number of iterations.
N = 10
print(timeit.timeit("dot.dotProductLoop(a, b)", globals=globals(), number=N))

# Ask to repeat measurements 5 times and return a list of the runtimes.
print(timeit.repeat("dot.dotProductLoop(a, b)", globals=globals(), number=N, repeat=5))

# Use auto-range to discover a reasonable number of iterations automatically.
print(timeit.Timer("dot.dotProductLoop(a, b)", globals=globals()).autorange())


def autobench(stmt, globals=None, repeat=5):
    # Find the number of iterations to run
    timer = timeit.Timer(stmt, globals=globals)
    num, _ = timer.autorange()
    raw_timings = timer.repeat(repeat=repeat, number=num)
    best = min(raw_timings)
    print(f"{num} loops, best of {repeat}: {best/num:.3f}s per loop")


autobench("dot.dotProductLoop(a, b)", globals=globals())
autobench("dot.dotProductZip(a, b)", globals=globals())
autobench("dot.dotProductStarmap(a, b)", globals=globals())
