from bytecodejit import bytecodejit


@bytecodejit
def some_expr(a, b, c):
    return a + b + 1
    # return b / (a + 2) - c * (b - a)


some_expr(2, 16, 3)
