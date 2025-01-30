from tracejit import tracejit


@tracejit
def some_expr(a, b, c):
    # return b / (a + 2) - c * (b - a)
    return a + b


print(some_expr(2, 16, 3))
