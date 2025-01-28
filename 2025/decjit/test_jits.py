import pytest
from astjit import astjit
from bytecodejit import bytecodejit


def add(a, b):
    return a + b


def addwith2(a, b):
    return a + b + 2


def expr(a, b, c):
    return (a + b) * c - 100 / (c - 1)


@pytest.mark.parametrize("jitdec", [bytecodejit, astjit])
def test_jits(jitdec):
    jit_add = jitdec(add)
    assert jit_add(1, 2) == 3

    jit_addwith2 = jitdec(addwith2)
    assert jit_addwith2(1, 2) == 5
    assert jit_addwith2(10, 20) == 32

    jit_expr = jitdec(expr)
    assert jit_expr(1, 2, 3) == -41.0
    assert jit_expr(2, -1, 201) == 200.5
