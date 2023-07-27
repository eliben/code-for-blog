from math import sqrt

def gen_primes_upto(n):
    """Generates a sequence of primes < n.

        Uses the full sieve of Eratosthenes with O(n) memory.
    """
    if n == 2:
        return

    table = [True] * n
    sqrtn = int(sqrt(n))

    for i in range(2, sqrtn+1):
        if table[i]:
            for j in range(i*i, n, i):
                table[j] = False
    
    yield 2
    for i in range(3, n, 2):
        if table[i]:
            yield i
