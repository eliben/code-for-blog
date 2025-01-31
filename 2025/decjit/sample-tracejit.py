from tracejit import tracejit


@tracejit
def add(a, b):
    return a + b


print(add(1, 2))


@tracejit
def some_expr(a, b, c):
    return b / (a + 2) - c * (b - a)


print(some_expr(2, 16, 3))


# The tracing approach "sees through" locals, etc.
@tracejit
def use_locals(a, b, c):
    x = a + 2
    y = b - a
    z = c * x
    return y / x - z


print(use_locals(2, 8, 11))


# ... and loops!
@tracejit
def use_loop(a, b, c):
    result = 0
    for i in range(1, 11):
        result += i
    return result + b * c


print(use_loop(10, 2, 3))
