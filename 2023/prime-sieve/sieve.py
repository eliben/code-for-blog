from math import sqrt


def gen_primes_upto(n):
    """Generates a sequence of primes < n.

    Uses the full sieve of Eratosthenes with O(n) memory.
    """
    if n == 2:
        return

    table = [True] * n
    sqrtn = int(sqrt(n))

    for i in range(2, sqrtn + 1):
        if table[i]:
            for j in range(i * i, n, i):
                table[j] = False

    yield 2
    for i in range(3, n, 2):
        if table[i]:
            yield i


# TODO: measuring length of D -- insight:
# it has all the primes in it, so its size is at least O(number of primes),
# which is O(n / logn) per https://en.wikipedia.org/wiki/Prime-counting_function
# It makes sense because for a new number we have to check its divisibility
# by all the primes before it, so we have to store them somehow!

# Uncommenting the prints clarifies how D propagates

def gen_primes():
    """Generate an infinite sequence of prime numbers."""

    # Maps composites to primes witnessing their compositeness.
    # This is memory efficient, as the sieve is not "run forward"
    # indefinitely, but only as long as required by the current
    # number being tested.
    D = {}

    # The running integer that's checked for primeness
    q = 2

    while 1:
        print(f'-- {q}')
        print('len of D =', len(D))
        if q not in D:
            # q is a new prime.
            # Yield it and mark its first multiple that isn't
            # already marked in previous iterations
            D[q * q] = [q]
            # print(f'Setting D[{q*q}]=[{q}]')
            # print(sorted(D.items()))
            yield q
        else:
            # q is composite. D[q] is the list of primes that
            # divide it. Since we've reached q, we no longer
            # need it in the map, but we'll mark the next
            # multiples of its witnesses to prepare for larger
            # numbers
            for p in D[q]:
                D.setdefault(p + q, []).append(p)
            del D[q]
            # print(f'Removing D[{q}]')
            # print(sorted(D.items()))

        q += 1

if __name__ == '__main__':
    gen = gen_primes()
    for i in range(100000):
        p = next(gen)
        print(p)
