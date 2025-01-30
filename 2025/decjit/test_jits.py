import pytest
from astjit import astjit
from bytecodejit import bytecodejit
from tracejit import tracejit


def add(a, b):
    return a + b


def addwith2(a, b):
    return a + b + 2


def expr(a, b, c):
    return (a + b) * c - 100 / (c - 1)


def expr2(a, b, c, d):
    return (a + d) * (10 - c) + b + d / c


def expr_with_rev_ops(a, b, c):
    return (2 - a) + (3 * b) - (10 / c)


@pytest.mark.parametrize("jitdec", [bytecodejit, astjit, tracejit])
def test_jits(jitdec):
    jit_add = jitdec(add)
    assert jit_add(1, 2) == 3

    jit_addwith2 = jitdec(addwith2)
    assert jit_addwith2(1, 2) == 5
    assert jit_addwith2(10, 20) == 32

    jit_expr = jitdec(expr)
    assert jit_expr(1, 2, 3) == -41.0
    assert jit_expr(2, -1, 201) == 200.5

    jit_expr2 = jitdec(expr2)
    assert jit_expr2(1, 2, 5, 10) == 59.0
    assert jit_expr2(5, -5, 10, 40) == -1

    jit_expr_with_rev_ops = jitdec(expr_with_rev_ops)
    assert jit_expr_with_rev_ops(6, 9, 5) == 21
    assert jit_expr_with_rev_ops(-8, 4, -20) == 22.5
