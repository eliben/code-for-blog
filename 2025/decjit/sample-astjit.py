from astjit import astjit


@astjit
def some_expr(a, b, c):
    return b / (a + 2) - c * (b - a)


print(some_expr(2, 16, 3))
