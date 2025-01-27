from astjit import astjit


@astjit
def add(a, b):
    return a + b + 2


print(add(1, 2))
