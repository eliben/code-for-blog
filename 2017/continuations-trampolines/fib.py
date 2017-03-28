# Examples of multiple recursion with fibonacci.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

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


if __name__ == '__main__':
    print([fib_rec(i) for i in range(1, 11)])
    print([fib_almost_tail(i) for i in range(1, 11)])
    print([fib_tail(i) for i in range(1, 11)])
    print([fib_iterative(i) for i in range(1, 11)])

    #print(fib_rec(40))
