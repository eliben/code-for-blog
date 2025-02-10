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
