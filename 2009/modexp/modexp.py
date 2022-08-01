# Modular exponentiation algorithms.
#
# This code only works with Python 3.
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.

import sys
import time

def modexp_mul(a, b, n):
    r = 1
    for i in range(b):
        r = r * a % n
    return r


def _bits_of_n(n):
    """ Return the list of the bits in the binary representation
        of n, from LSB to MSB
    """
    bits = []

    while n:
        bits.append(n % 2)
        n //= 2

    return bits


def _digits_of_n(n, b):
    """ Return the list of the digits in the base 'b'
        representation of n, from LSB to MSB
    """
    digits = []

    while n:
        digits.append(n % b)
        n //= b

    return digits


def modexp_lr(a, b, n):
    r = 1
    for bit in reversed(_bits_of_n(b)):
        r = r * r % n
        if bit == 1:
            r = r * a % n
    return r


def modexp_lr_k_ary(a, b, n, k=5):
    """ Compute a ** b (mod n)

        K-ary LR method, with a customizable 'k'.
    """
    base = 2 << (k - 1)

    # Precompute the table of exponents
    table = [1] * base
    for i in range(1, base):
        table[i] = table[i - 1] * a % n

    # Just like the binary LR method, just with a
    # different base
    #
    r = 1
    for digit in reversed(_digits_of_n(b, base)):
        for i in range(k):
            r = r * r % n
        if digit:
            r = r * table[digit] % n

    return r


def modexp_rec(a, b, n):
    if b == 0:
        return 1
    elif b & 1:
        return a * modexp_rec(a, b - 1, n) % n
    else:
        p = modexp_rec(a, b // 2, n)
        return  p * p % n


def modexp_rl(a, b, n):
    r = 1
    while 1:
        if b % 2 == 1:
            r = r * a % n
        b //= 2
        if b == 0:
            break
        a = a * a % n

    return r


from random import randint

# for small b, RL is slower. for larger b, it's faster

N = 100
ays = [randint(10 ** 40, 10 ** 96) for i in range(N)]
bys = [randint(10 ** 40, 10 ** 96) for i in range(N)]
nys = [randint(10 ** 40, 10 ** 96) for i in range(N)]

def time_modexp_func(func, niters):
    s = 0
    tic = time.perf_counter()
    for i in range(niters):
        s += func(ays[i], bys[i], nys[i])
    toc = time.perf_counter()
    print(f'{func.__name__}: {toc - tic:0.4f} seconds')
    print('s =', s % 99999999999999999999999999)


time_modexp_func(modexp_lr, N)
time_modexp_func(modexp_lr_k_ary, N)
time_modexp_func(pow, N)
