from itertools import *
from sieve import *

import unittest


class TestSieve(unittest.TestCase):
    def _test_gen_primes_upto_variant(self, func):
        self.assertEqual(list(func(2)), [])
        self.assertEqual(list(func(3)), [2])
        self.assertEqual(list(func(4)), [2, 3])
        self.assertEqual(list(func(6)), [2, 3, 5])
        self.assertEqual(list(func(7)), [2, 3, 5])
        self.assertEqual(list(func(8)), [2, 3, 5, 7])
        self.assertEqual(list(func(9)), [2, 3, 5, 7])
        self.assertEqual(list(func(10)), [2, 3, 5, 7])
        self.assertEqual(list(func(13)), [2, 3, 5, 7, 11])
        self.assertEqual(list(func(14)), [2, 3, 5, 7, 11, 13])
        self.assertEqual(list(func(15)), [2, 3, 5, 7, 11, 13])
        self.assertEqual(list(func(16)), [2, 3, 5, 7, 11, 13])
        self.assertEqual(list(func(17)), [2, 3, 5, 7, 11, 13])
        self.assertEqual(list(func(18)), [2, 3, 5, 7, 11, 13, 17])
        self.assertEqual(list(func(19)), [2, 3, 5, 7, 11, 13, 17])
        self.assertEqual(list(func(20)), [2, 3, 5, 7, 11, 13, 17, 19])
        self.assertEqual(list(func(21)), [2, 3, 5, 7, 11, 13, 17, 19])

        # Obtained by dowloading the list of the first 100k primes from
        # https://www.mathsisfun.com/numbers/prime-number-lists.html
        # and summing the list on the command line.
        self.assertEqual(sum(gen_primes_upto(99992)), 454396537)

    def test_gen_primes_upto(self):
        self._test_gen_primes_upto_variant(gen_primes_upto)

    def test_gen_primes_upto_segmented(self):
        self._test_gen_primes_upto_variant(gen_primes_upto_segmented)

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

    def test_gen_primes(self):
        self._test_gen_primes_variant(gen_primes)
    
    def test_gen_primes_opt(self):
        self._test_gen_primes_variant(gen_primes_opt)


if __name__ == "__main__":
    unittest.main()
