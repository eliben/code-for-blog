from itertools import *
from sieve import *

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

    def test_gen_primes(self):
        self._test_gen_primes_variant(gen_primes)
    
    def test_gen_primes_opt(self):
        self._test_gen_primes_variant(gen_primes_opt)

    def _test_gen_primes_variant(self, func):
        # fist 20 primes
        primes20 = [
            2,
            3,
            5,
            7,
            11,
            13,
            17,
            19,
            23,
            29,
            31,
            37,
            41,
            43,
            47,
            53,
            59,
            61,
            67,
            71,
        ]
        self.assertEqual(list(islice(func(), 20)), primes20)
        self.assertEqual(list(gen_primes_upto(72)), primes20)

        # Sanity check adding up the first 1500 primes, comparing gen_primes
        # to gen_primes_upto.
        # (12553 is the 1500th prime)
        self.assertEqual(sum(islice(func(), 1500)), sum(gen_primes_upto(12554)))

        # Another sanity check, summing all primes up to 100,000
        self.assertEqual(
            sum(takewhile(lambda x: x < 100000, func())),
            sum(gen_primes_upto(100000)),
        )

if __name__ == "__main__":
    unittest.main()
