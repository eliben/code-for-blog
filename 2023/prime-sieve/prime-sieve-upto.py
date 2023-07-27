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


import unittest

class TestSieve(unittest.TestCase):
    def test_gen_primes_upto(self):
        self.assertEqual(list(gen_primes_upto(2)), [])
        self.assertEqual(list(gen_primes_upto(3)), [2])
        self.assertEqual(list(gen_primes_upto(4)), [2, 3])
        self.assertEqual(list(gen_primes_upto(6)), [2, 3, 5])
        self.assertEqual(list(gen_primes_upto(7)), [2, 3, 5])
        self.assertEqual(list(gen_primes_upto(8)), [2, 3, 5, 7])
        self.assertEqual(list(gen_primes_upto(9)), [2, 3, 5, 7])
        self.assertEqual(list(gen_primes_upto(10)), [2, 3, 5, 7])
        self.assertEqual(list(gen_primes_upto(13)), [2, 3, 5, 7, 11])
        self.assertEqual(list(gen_primes_upto(14)), [2, 3, 5, 7, 11, 13])

unittest.main()