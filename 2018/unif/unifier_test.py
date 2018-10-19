import unittest

from unifier import *


class TestParser(unittest.TestCase):
    def assertParsed(self, s, exprstr):
        """Parses s and checks that the parsed representation == exprstr."""
        self.assertEqual(str(parse_expr(s)), exprstr)

    def test_basics(self):
        self.assertParsed('foo', 'foo')
        self.assertParsed('FOO', '$FOO')
        self.assertParsed('4', '4')
        self.assertParsed('foo(4)', 'foo(4)')
        self.assertParsed('foo(4, 10)', 'foo(4,10)')

    def test_nestings(self):
        self.assertParsed('from(joe, FOO, 4)', 'from(joe,$FOO,4)')
        self.assertParsed('apply(BANG, full(10), 20)', 'apply($BANG,full(10),20)')
        self.assertParsed('f(g(h(X)))', 'f(g(h($X)))')
        self.assertParsed('f(g(h(X)),p(VOVO))', 'f(g(h($X)),p($VOVO))')


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

    def test_bindings(self):
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


if __name__ == '__main__':
    unittest.main()
