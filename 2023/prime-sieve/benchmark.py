import sieve
import timeit


setup = '''
from itertools import islice
from sieve import gen_primes_upto, gen_primes   
'''

def bench_dict():
    gp = sieve.gen_primes()
    for i in range(9000):
        next(gp)

def bench_upto():
    gp = sieve.gen_primes_upto(100000)
    for i in range(9000):
        next(gp)

t1 = timeit.Timer("bench_upto()", globals=locals())
print("gen_primes_upto", t1.repeat(repeat=3, number=100))

t2 = timeit.Timer("bench()", globals=locals())
print("gen_primes", t1.repeat(repeat=3, number=100))


# print(timeit.timeit("bench_upto()", globals=locals(), number=100))

