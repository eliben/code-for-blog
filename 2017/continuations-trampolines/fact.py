# Factorial - recursive, tail recursive, CPS, trampolined.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

import tracing

# Remove the tracing.TraceCalls() decorators from functions to avoid tracing.

@tracing.TraceCalls()
def fact_rec(n):
    if n == 0:
        return 1
    else:
        return n * fact_rec(n - 1)


def fact_tailrec(n, result=1):
    if n == 0:
        return result
    else:
        return fact_tailrec(n - 1, result * n)


def fact_cps(n, cont):
    if n == 0:
        return cont(1)
    else:
        return fact_cps(n - 1, lambda value: cont(n * value))


end_cont = lambda value: value

def fact_cps_main(n):
    return fact_cps(n, end_cont)


@tracing.TraceCalls()
def fact_cps_thunked(n, cont):
    if n == 0:
        return cont(1)
    else:
        return lambda: fact_cps_thunked(
                         n - 1,
                         lambda value: lambda: cont(n * value))


@tracing.TraceCalls()
def trampoline(f, *args):
    v = f(*args)
    while callable(v):
        v = v()
    return v


if __name__ == '__main__':
    import sys
    print('Recursion limit is', sys.getrecursionlimit())

    print(fact_rec(6))
    print(trampoline(fact_cps_thunked, 1000, end_cont))
    print(fact_tailrec(19))

    print(fact_cps_main(19))
