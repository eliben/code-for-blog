from astjit import astjit


@astjit
def addwith2(a, b):
    return a + b + 2


@astjit
def expr(a, b, c):
    return (a + b) * c - 100 / (c - 1)


def test_astjit_add():
    assert addwith2(1, 2) == 5
    assert addwith2(10, 20) == 32


def test_astjit_expr():
    assert expr(1, 2, 3) == -41.0
    assert expr(2, -1, 201) == 200.5
