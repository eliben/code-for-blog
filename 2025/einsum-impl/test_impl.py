import numpy as np
from impl import translate_einsum


def run_test(subscript, *args, show_func=False):
    namespace = {"np": np}
    if show_func:
        print("\n" + translate_einsum(subscript))
    exec(translate_einsum(subscript), namespace)
    got = namespace["calc"](*args)
    want = np.einsum(subscript, *args)
    assert np.allclose(got, want)


def test_translate_einsum():
    A = np.arange(6).reshape(2, 3)
    B = np.arange(12).reshape(3, 4) + 1

    # Basic matmul
    run_test("ij,jk->ik", A, B)

    # Try the same with some whitespace in the subscript
    run_test("ij, jk -> ik", A, B)
    run_test("  ij, jk -> ik", A, B)
    run_test("  ij, jk  -> ik  ", A, B)

    # Transpose output
    run_test("ij,jk->ki", A, B)

    # Tranpose input
    run_test("ij,kj->ik", A, A)

    # Not matmul because of different indices: this is a double loop sum
    run_test("ij,pk->ik", A, B)

    # Outer product
    v1 = np.arange(8)
    v2 = np.arange(5) + 2
    run_test("i,j->ij", v1, v2)

    # Batched matmuls
    Ab = np.arange(6 * 6).reshape(6, 2, 3)
    Bb = np.arange(6 * 12).reshape(6, 3, 4)

    run_test("bmd,bdn->bmn", Ab, Bb)

    # Two batch dimensions
    m = 4
    d = 3
    k = 6
    h = 5
    b = 10
    Pk = np.random.randn(h, d, k)
    M = np.random.randn(b, m, d)

    run_test("bmd,hdk->bhmk", M, Pk)
    run_test("bmd,hdk->hbmk", M, Pk)

    # Contraction in more than one dimension
    b = 10
    n = 4
    d = 3
    v = 6
    h = 5
    Ox = np.random.randn(b, h, n, v)
    Po = np.random.randn(h, d, v)
    run_test("bhnv,hdv->bnd", Ox, Po)

    # More than two input arrays
    C = np.arange(20).reshape(4, 5)
    run_test("ij,jk,kl->il", A, B, C)
