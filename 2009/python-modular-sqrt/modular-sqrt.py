# Computing modular square roots in Python.
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
import math
import random


def modular_sqrt(a, p):
    """Find a quadratic residue (mod p) of 'a'. p
    must be an odd prime.

    Solve the congruence of the form:
        x^2 = a (mod p)
    And returns x. Note that p - x is also a root.

    0 is returned is no square root exists for
    these a and p.

    The Shanks-Tonelli algorithm is used (except
    for some simple cases in which the solution
    is known from an identity). This algorithm
    runs in polynomial time (unless the
    generalized Riemann hypothesis is false).
    """
    # Simple cases
    #
    if legendre_symbol(a, p) != 1:
        return 0
    elif a == 0:
        return 0
    elif p == 2:
        return 0
    elif p % 4 == 3:
        return pow(a, (p + 1) // 4, p)

    # Partition p-1 to s * 2^e for an odd s (i.e.
    # reduce all the powers of 2 from p-1)
    #
    s = p - 1
    e = 0
    while s % 2 == 0:
        s //= 2
        e += 1

    # Find some 'n' with a legendre symbol n|p = -1.
    # Shouldn't take long.
    #
    n = 2
    while legendre_symbol(n, p) != -1:
        n += 1

    # Here be dragons!
    # Read the paper "Square roots from 1; 24, 51,
    # 10 to Dan Shanks" by Ezra Brown for more
    # information
    #

    # x is a guess of the square root that gets better
    # with each iteration.
    # b is the "fudge factor" - by how much we're off
    # with the guess. The invariant x^2 = ab (mod p)
    # is maintained throughout the loop.
    # g is used for successive powers of n to update
    # both a and b
    # r is the exponent - decreases with each update
    #
    x = pow(a, (s + 1) // 2, p)
    b = pow(a, s, p)
    g = pow(n, s, p)
    r = e

    while True:
        t = b
        m = 0
        for m in range(r):
            if t == 1:
                break
            t = pow(t, 2, p)

        if m == 0:
            return x

        gs = pow(g, 2 ** (r - m - 1), p)
        g = (gs * gs) % p
        x = (x * gs) % p
        b = (b * g) % p
        r = m


def legendre_symbol(a, p):
    """Compute the Legendre symbol a|p using
    Euler's criterion. p is a prime, a is
    relatively prime to p (if p divides
    a, then a|p = 0)

    Returns 1 if a has a square root modulo
    p, -1 otherwise.
    """
    ls = pow(a, (p - 1) // 2, p)
    return -1 if ls == p - 1 else ls


def gen_primes_upto(n):
    """Generates a sequence of primes < n.

    Uses the full sieve of Eratosthenes with O(n) memory.
    """
    if n == 2:
        return

    table = [True] * n
    sqrtn = int(math.ceil(math.sqrt(n)))

    for i in range(2, sqrtn):
        if table[i]:
            for j in range(i * i, n, i):
                table[j] = False

    yield 2
    for i in range(3, n, 2):
        if table[i]:
            yield i


# When this file is run, run the tests.
if __name__ == "__main__":
    import unittest

    class TestModularSqrt(unittest.TestCase):
        def test_modular_sqrt(self):
            # Smoke-testing with some known values
            self.assertEqual(modular_sqrt(4, 5), 3)
            self.assertEqual(modular_sqrt(12, 13), 8)

            # Randomized stress testing
            primes = list(gen_primes_upto(100000))[10:]

            # 100 tests
            for i in range(100):
                n = random.randint(1, 100000000)
                p = random.choice(primes)

                # Compute the residue of n^2 (mod p)
                sq_residue = pow(n, 2, p)

                # Now the result of the function under test.
                # Note that since the answer is not unique, we
                # can't just compare this to n
                mod_sqrt = modular_sqrt(sq_residue, p)

                # Raise mod_sqrt^2 (mod p) to verify that the
                # result is correct
                res = pow(mod_sqrt, 2, p)
                self.assertEqual(res, sq_residue)

    unittest.main()
