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


if __name__ == '__main__':
    unittest.main()
