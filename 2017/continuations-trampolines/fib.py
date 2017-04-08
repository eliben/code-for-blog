# Examples of multiple recursion with fibonacci, CPS and trampolining.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

import tracing


def fib_rec(n):
    if n < 2:
        return 1
    else:
        return fib_rec(n - 1) + fib_rec(n - 2)


def fib_almost_tail(n, result=1):
    if n < 2:
        return result
    prev2 = fib_almost_tail(n - 2)
    return fib_almost_tail(n - 1, prev2 + result)


def fib_tail(n, accum1=1, accum2=1):
    if n < 2:
        return accum1
    else:
        return fib_tail(n - 1, accum1 + accum2, accum1)


def fib_iterative(n):
    accum1, accum2 = 1, 1
    accum2 = 1
    while n >= 2:
        n -= 1
        accum1, accum2 = accum1 + accum2, accum1
    return accum1


# CPS transform partially applied.
def fib_cps_partial(n, cont):
    if n < 2:
        return cont(1)
    else:
        return fib_cps_partial(
                n - 1,
                lambda value: value + fib_cps_partial(n - 2, cont))


# CPS transform fully applied.
def fib_cps(n, cont):
    if n < 2:
        return cont(1)
    else:
        return fib_cps(
                 n - 1,
                 lambda value: fib_cps(
                                 n - 2,
                                 lambda value2: cont(value + value2)))


@tracing.TraceCalls()
def fib_cps_thunked(n, cont):
    if n < 2:
        return cont(1)
    else:
        return lambda: fib_cps_thunked(
                         n - 1,
                         lambda value:
                           lambda: fib_cps_thunked(
                                     n - 2,
                                     lambda value2:
                                       lambda: cont(value + value2)))


@tracing.TraceCalls()
def trampoline(f, *args):
    v = f(*args)
    while callable(v):
        v = v()
    return v


end_cont = lambda value: value


if __name__ == '__main__':
    print([fib_rec(i) for i in range(1, 11)])
    print([fib_almost_tail(i) for i in range(1, 11)])
    print([fib_tail(i) for i in range(1, 11)])
    print([fib_iterative(i) for i in range(1, 11)])

    print([fib_cps_partial(i, end_cont) for i in range(1, 11)])
    print([fib_cps(i, end_cont) for i in range(1, 11)])

    print([trampoline(fib_cps_thunked, i, end_cont) for i in range(1, 11)])
