from itertools import starmap
from operator import mul


def dotProductLoop(a, b):
    result = 0
    for i in range(len(a)):
        result += a[i] * b[i]
    return result


def dotProductZip(a, b):
    return sum(x * y for x, y in zip(a, b))


def dotProductStarmap(a, b):
    return sum(starmap(mul, zip(a, b)))
