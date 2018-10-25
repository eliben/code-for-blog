import unittest

from unifier import *


class TestParser(unittest.TestCase):
    def assertParsed(self, s, termstr):
        """Parses s and checks that the parsed representation == termstr."""
        self.assertEqual(str(parse_term(s)), termstr)

    def test_basics(self):
        self.assertParsed('foo', 'foo')
        self.assertParsed('FOO', 'FOO')
        self.assertParsed('4', '4')
        self.assertParsed('foo(4)', 'foo(4)')
        self.assertParsed('foo(4, 10)', 'foo(4,10)')

    def test_nestings(self):
        self.assertParsed('from(joe, FOO, 4)', 'from(joe,FOO,4)')
        self.assertParsed('apply(BANG, full(10), 20)', 'apply(BANG,full(10),20)')
        self.assertParsed('f(g(h(X)))', 'f(g(h(X)))')
        self.assertParsed('f(g(h(X)),p(VOVO))', 'f(g(h(X)),p(VOVO))')


class TestOccursCheck(unittest.TestCase):
    def test_basics(self):
        self.assertTrue(occurs_check(Var('K'), Var('K'), {}))
        self.assertFalse(occurs_check(Var('K'), Var('T'), {}))
        self.assertFalse(occurs_check(Var('K'), Const('t'), {}))

    def test_app_args(self):
        self.assertTrue(
            occurs_check(Var('O'),
                         App('joe', (Var('O'), Var('P'))), {}))
        self.assertTrue(
            occurs_check(Var('O'),
                         App('joe', (Var('G'), Var('O'), Var('P'))), {}))
        self.assertFalse(
            occurs_check(Var('U'),
                         App('joe', (Var('G'), Var('O'), Var('P'))), {}))

    def test_subst(self):
        self.assertTrue(
            occurs_check(Var('O'),
                         Var('B'),
                         {'B': Var('O')}))
        self.assertFalse(
            occurs_check(Var('O'),
                         Var('B'),
                         {'B': Var('OB')}))
        self.assertTrue(
            occurs_check(Var('O'),
                         App('joe', (Var('B'),)),
                         {'B': Var('O')}))
        self.assertTrue(
            occurs_check(Var('O'),
                         App('joe', (Var('B'),)),
                         {'B': Var('D'), 'D': Var('O')}))
        self.assertFalse(
            occurs_check(Var('O'),
                         App('joe', (Var('B'),)),
                         {'B': Var('D'), 'D': Const('O')}))


def rebind_sorted(subst):
    """Given subst, rebinds vars to be lexicographically sorted.

    For example, the subst {'V': Var('X')} and {'X': Var('V')} are logically
    equivalent but won't compare as equal. This function takes a subst and
    reorders all such subst (where the value is a var) such that
    key <= value.name.
    """
    newsubst = {}
    for k, v in subst.items():
        # Don't overwrite subst for v.name if it's already bound.
        if isinstance(v, Var) and v.name not in subst and k > v.name:
            newsubst[v.name] = Var(k)
        else:
            newsubst[k] = v
    return newsubst


class TestUnify(unittest.TestCase):

    def assertUnifyResult(self, s1, s2, result):
        """Asserts that the unify result of s1 and s2 is result.

        s1 and s2 are string representatios of terms; result is the
        expected subst dict.
        """
        subst = unify(parse_term(s1), parse_term(s2), {})
        if subst is None:
            self.assertIsNone(result, msg='Expected result=None since subst=None')
        else:
            self.assertDictEqual(rebind_sorted(subst), rebind_sorted(result))

    def test_basic_var(self):
        self.assertUnifyResult('v', 't', None)
        self.assertUnifyResult('V', 't', {'V': Const('t')})
        self.assertUnifyResult('t', 'V', {'V': Const('t')})
        self.assertUnifyResult('T', 'V', {'T': Var('V')})

    def test_basic_app(self):
        self.assertUnifyResult('f(v)', 'g(v)', None)
        self.assertUnifyResult('f(v)', 'f(v)', {})
        self.assertUnifyResult('f(V)', 'f(v)', {'V': Const('v')})
        self.assertUnifyResult('f(v)', 'f(V)', {'V': Const('v')})

        self.assertUnifyResult('f(v, x)', 'f(V, x)', {'V': Const('v')})
        self.assertUnifyResult('f(x, v)', 'f(x, V)', {'V': Const('v')})

        self.assertUnifyResult('f(v, x)', 'f(V, y)', None)
        self.assertUnifyResult('f(y, v)', 'f(x, V)', None)
        self.assertUnifyResult('f(y, v)', 'y', None)
        self.assertUnifyResult('f(v)', 'Y',
                {'Y': App('f', (Const('v'),))})
        self.assertUnifyResult('f(y, v)', 'Y',
                {'Y': App('f', (Const('y'), Const('v')))})

        self.assertUnifyResult('f(X, X)', 'f(Y, Y)', {'X': Var('Y')})
        self.assertUnifyResult('f(Y, Y)', 'f(X, X)', {'X': Var('Y')})
        self.assertUnifyResult('f(Y, X)', 'f(X, Y)', {'X': Var('Y')})
        self.assertUnifyResult('f(X, X, X)', 'f(Y, Y, Y)', {'X': Var('Y')})
        self.assertUnifyResult('f(Y, X, Y)', 'f(X, Y, X)', {'X': Var('Y')})
        self.assertUnifyResult('f(X, Y, A)', 'f(Y, X, X)',
                {'A': Var('Y'), 'X': Var('Y')})
        self.assertUnifyResult('f(g(X, Y, A), g(Y, X, X))', 'f(Z, Z)',
                {'A': Var('Y'), 'X': Var('Y'),
                 'Z': App('g', (Var('X'), Var('Y'), Var('A')))})

        self.assertUnifyResult('f(p, X, Y)', 'f(X, Y, X)',
                {'X': Const('p'), 'Y': Const('p')})
        self.assertUnifyResult('f(Y, X, Y)', 'f(X, Y, p)',
                {'X': Const('p'), 'Y': Var('X')})

        self.assertUnifyResult('f(X, h(X), Y, g(Y))', 'f(g(Z), W, Z, X)',
                {'X': App('g', (Var('Z'),)), 'W': App('h', (Var('X'),)), 'Y': Var('Z')})

        self.assertUnifyResult('f(X, X)', 'f(g(a, b, Y), g(a, Z, c))',
                {'X': App('g', (Const('a'), Const('b'), Var('Y'))),
                 'Y': Const('c'),
                 'Z': Const('b')})


class TestApplyUnifier(unittest.TestCase):

    def assertUnifier(self, s1, s2, result):
        """Asserts that the unifier term of s1 and s2 is result.

        All arguments are string representations of terms.
        """
        subst = unify(parse_term(s1), parse_term(s2), {})
        if subst is None:
            self.fail('expected {} and {} to unify'.format(s1, s2))
        unified_s1 = apply_unifier(parse_term(s1), subst)
        unified_s2 = apply_unifier(parse_term(s2), subst)
        self.assertEqual(unified_s1, unified_s2)
        self.assertEqual(unified_s1, parse_term(result))

    def test_unifier(self):
        self.assertUnifier('f(X)', 'f(t)', 'f(t)')
        self.assertUnifier('f(t)', 'f(X)', 'f(t)')
        self.assertUnifier('f(X, h(X), Y, g(Y))', 'f(g(a), W, a, X)',
                           'f(g(a),h(g(a)),a,g(a))')
        self.assertUnifier('f(X, h(X), Y, g(Y))', 'f(g(Z), W, Z, X)',
                           'f(g(Z),h(g(Z)),Z,g(Z))')


if __name__ == '__main__':
    unittest.main()
