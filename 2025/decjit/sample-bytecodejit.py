from bytecodejit import bytecodejit


@bytecodejit
def some_expr(a, b, c):
    return a + b + 2
    # return b / (a + 2) - c * (b - a)


print(some_expr(2, 16, 3))
