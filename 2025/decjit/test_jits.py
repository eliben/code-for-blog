from astjit import astjit


@astjit
def add(a, b):
    return a + b + 2


def test_astjit_add():
    assert add(1, 2) == 5
