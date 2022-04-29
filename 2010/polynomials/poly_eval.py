#-------------------------------------------------------------------------------
# Polynomial evaluation in Python
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
from timeit import Timer
import timeit


def poly_naive(A, x):
    p = 0
    for i, a in enumerate(A):
        p += (x ** i) * a
    return p


def poly_iter(A, x):
    p = 0
    xn = 1
    for a in A:
        p += xn * a
        xn *= x
    return p


def poly_horner(A, x):
    p = A[-1]
    i = len(A) - 2
    while i >= 0:
        p = p * x + A[i]
        i -= 1
    return p


testp = [2, -6, 5, 4, 3, -2, 0, 4]
testx = 3

print(poly_naive(testp, testx))
print(poly_iter(testp, testx))
print(poly_horner(testp, testx))

N = 1000

setup = r'''
from __main__ import poly_naive, poly_iter, poly_horner
import random

A = [random.randint(-20, 20) for i in range(500)]
x = 3
'''

print("Naive:", timeit.Timer(stmt='poly_naive(A, x)', setup=setup).timeit(N))
print("Iter:", timeit.Timer(stmt='poly_iter(A, x)', setup=setup).timeit(N))
print("Horner:", timeit.Timer(stmt='poly_horner(A, x)', setup=setup).timeit(N))
