# Recursive versions of factorial.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

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

end_cont = lambda value: value

def fact_cps(n, cont):
    if n == 0:
        return cont(1)
    else:
        return fact_cps(n - 1, lambda value: cont(n * value))


def fact_cps_main(n):
    return fact_cps(n, end_cont)


if __name__ == '__main__':
    import sys
    print('Recursion limit is', sys.getrecursionlimit())

    print(fact_rec(19))
    print(fact_tailrec(19))

    print(fact_cps_main(19))
