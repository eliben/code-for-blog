# Prime sieves.
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
import math
import itertools


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


def gen_primes_upto_segmented(n):
    """Generates a sequence of primes < n.

    Uses the segmented sieve or Eratosthenes algorithm with O(√n) memory.
    """
    # Simplify boundary cases by hard-coding some small primes.
    if n < 11:
        for p in [2, 3, 5, 7]:
            if p < n:
                yield p
        return

    # We break the range [0..n) into segments of size √n
    segsize = int(math.ceil(math.sqrt(n)))

    # Find the primes in the first segment by calling the basic sieve on that
    # segment (its memory usage will be O(√n)). We'll use these primes to
    # sieve all subsequent segments.
    baseprimes = list(gen_primes_upto(segsize))
    for bp in baseprimes:
        yield bp

    for segstart in range(segsize, n, segsize):
        # Create a new table of size √n for each segment; the old table
        # is thrown away, so the total memory use here is √n
        # seg[i] represents the number segstart+i
        seg = [True] * segsize

        for bp in baseprimes:
            # The first multiple of bp in this segment can be calculated using
            # modulo.
            first_multiple = (
                segstart if segstart % bp == 0 else segstart + bp - segstart % bp
            )
            # Mark all multiples of bp in the segment as composite.
            for q in range(first_multiple, segstart + segsize, bp):
                seg[q % len(seg)] = False

        # Sieving is done; yield all composites in the segment (iterating only
        # over the odd ones).
        start = 1 if segstart % 2 == 0 else 0
        for i in range(start, len(seg), 2):
            if seg[i]:
                if segstart + i >= n:
                    break
                yield segstart + i


def gen_primes():
    """Generate an infinite sequence of primes."""
    # Maps composites to primes witnessing their compositeness.
    D = {}

    # The running integer that's checked for primeness
    q = 2

    while True:
        if q not in D:
            # q is a new prime.
            # Yield it and mark its first multiple that isn't
            # already marked in previous iterations
            D[q * q] = [q]
            yield q
        else:
            # q is composite. D[q] holds some of the primes that
            # divide it. Since we've reached q, we no longer
            # need it in the map, but we'll mark the next
            # multiples of its witnesses to prepare for larger
            # numbers
            for p in D[q]:
                D.setdefault(p + q, []).append(p)
            del D[q]

        q += 1


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
        else:
            x = q + p + p  # get odd multiples
            while x in D:
                x += p + p
            D[x] = p


if __name__ == "__main__":
    gen = gen_primes()
    for i in range(30):
        p = next(gen)
        print(p)
