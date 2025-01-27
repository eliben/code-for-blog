from astjit import astjit

@astjit
def add(a, b):
    return a + b

print(add(1, 2))
