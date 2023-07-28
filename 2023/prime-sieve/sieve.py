import math
import itertools


def gen_primes_upto(n):
    """Generates a sequence of primes < n.

    Uses the full sieve of Eratosthenes with O(n) memory.
    """
    if n == 2:
        return

    table = [True] * n
    sqrtn = int(math.sqrt(n))

    for i in range(2, sqrtn + 1):
        if table[i]:
            for j in range(i * i, n, i):
                table[j] = False

    yield 2
    for i in range(3, n, 2):
        if table[i]:
            yield i


def gen_primes_upto_segmented(n):
    """Generates a sequence of primes < n.

    Uses the segmented sieve or Eratosthenes algorithm with O(sqrt(n)) memory.
    """
    # Simplify boundary cases by hard-coding for small n.
    if n < 10:
        for p in [2, 3, 5, 7]:
            if p < n:
                yield p
        return

    # Segment size
    segsize = int(math.ceil(math.sqrt(n)))

    # List of primes < sqrt(n) which we'll use to sieve all segments
    baseprimes = list(gen_primes_upto(segsize))
    # print('bp', baseprimes)

    for bp in baseprimes:
        yield bp

    for segstart in range(segsize, n, segsize):
        seg = [True] * segsize

        segend = segstart + segsize

        # print(f'seg start={segstart}, segend={segend}')
        for bp in baseprimes:
            first_multiple = (
                segstart if segstart % bp == 0 else segstart + bp - segstart % bp
            )
            for q in range(first_multiple, segend, bp):
                seg[q % len(seg)] = False

        start = 1 if segstart % 2 == 0 else 0
        for i in range(start, len(seg), 2):
            if seg[i]:
                if segstart + i >= n:
                    break
                yield segstart + i


# TODO: measuring length of D -- insight:
# it has all the primes in it, so its size is at least O(number of primes),
# which is O(n / logn) per https://en.wikipedia.org/wiki/Prime-counting_function
# each prime appears only in one list (one value of D)
# This makes sense because to check any number we need access to all primes
# smaller than it, so they have to be *somewhere*.
# An optimization would be only keep the primes below its square root, which
# is what the segmentation optimizations are about


def gen_primes():
    """Generate an infinite sequence of prime numbers."""

    # Maps composites to primes witnessing their compositeness.
    D = {}

    # The running integer that's checked for primeness
    q = 2

    while True:
        # print(f"-- {q}")
        # print("len of D =", len(D))
        if q not in D:
            # q is a new prime.
            # Yield it and mark its first multiple that isn't
            # already marked in previous iterations
            D[q * q] = [q]
            # print(f"Setting D[{q*q}]=[{q}]")
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
            # print(D)
            # print('len =', len(D), 'total size', sum(map(len, D.values())))
            # print(f"Removing D[{q}]")
            # print(sorted(D.items()))
            # print(sum(map(len, D.values())))

        q += 1


# See Will Neiss's comment: relevant to segmented sieve (wikipedia) ?
# And this tim peters answer: https://stackoverflow.com/a/19391111/8206
# a bit more algorithm details on the answer above it
# ... also related to the wheels mentioned in the paper
# https://research.cs.wisc.edu/techreports/1990/TR909.pdf


def gen_primes_opt():
    # combines martelli's, hochberg's and beinecke optimizations from
    # https://code.activestate.com/recipes/117119-sieve-of-eratosthenes/#c2
    yield 2

    D = {}

    for q in itertools.count(3, step=2):
        p = D.pop(q, None)
        if not p:
            D[q * q] = q
            yield q
            # print(D)
        else:
            x = q + p + p  # get odd multiples
            while x in D:
                x += p + p
            D[x] = p
            # print('total size', len(D))


if __name__ == "__main__":
    # gen = gen_primes()
    # for i in range(30):
    #     p = next(gen)
    #     print(p)

    gg = gen_primes_upto_segmented(3)
    print(list(gg))
