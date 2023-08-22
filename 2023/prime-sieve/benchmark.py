# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
import sieve
import timeit


setup = '''
from itertools import islice
from sieve import gen_primes_upto, gen_primes   
'''

def bench():
    gp = sieve.gen_primes()
    for i in range(9000):
        next(gp)


def bench_opt():
    gp = sieve.gen_primes_opt()
    for i in range(9000):
        next(gp)


def bench_upto():
    # Note: for gen_primes_upto, we need to know ahead of time what limit to
    # set. We happen to know that the first 9000 primes are < 100000, but only
    # barely. If the limit is set too high, gen_primes_upto will do lots of
    # unnecessary work.
    gp = sieve.gen_primes_upto(100000)
    for i in range(9000):
        next(gp)


def bench_upto_segmented():
    gp = sieve.gen_primes_upto_segmented(100000)
    for i in range(9000):
        next(gp)


t1 = timeit.Timer("bench_upto()", globals=locals())
print("gen_primes_upto", t1.repeat(repeat=3, number=100))

t11 = timeit.Timer("bench_upto_segmented()", globals=locals())
print("gen_primes_upto_segmented", t11.repeat(repeat=3, number=100))

t2 = timeit.Timer("bench()", globals=locals())
print("gen_primes", t2.repeat(repeat=3, number=100))

t3 = timeit.Timer("bench_opt()", globals=locals())
print("gen_primes_opt", t3.repeat(repeat=3, number=100))
