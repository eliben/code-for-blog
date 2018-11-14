# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import unittest

from parser import Parser


class TestParser(unittest.TestCase):
    def assertParsed(self, s, declstr):
        """Helper for testing parsed code.

        Parses s into an AST node and asserts that its string representation
        is equal to declstr.
        """
        p = Parser()
        node = p.parse_decl(s)
        self.assertEqual(str(node), declstr)

    def test_basic_decls(self):
        self.assertParsed('foo x = 2', 'Decl(foo, Lambda([x], 2))')
        self.assertParsed('foo x = false', 'Decl(foo, Lambda([x], false))')
        self.assertParsed('foo x = joe', 'Decl(foo, Lambda([x], joe))')
        self.assertParsed('foo x = (joe)', 'Decl(foo, Lambda([x], joe))')

    def test_parse_multiple(self):
        p = Parser()
        n1 = p.parse_decl('foo x = 10')
        self.assertEqual(str(n1), 'Decl(foo, Lambda([x], 10))')
        n2 = p.parse_decl('foo y = true')
        self.assertEqual(str(n2), 'Decl(foo, Lambda([y], true))')

    def test_basic_ifexpr(self):
        self.assertParsed('foo x = if y then z else q',
                          'Decl(foo, Lambda([x], If(y, z, q)))')

    def test_basic_op(self):
        self.assertParsed('bar z y = z + y',
                          'Decl(bar, Lambda([z, y], (z + y)))')

    def test_basic_proc(self):
        self.assertParsed('bar z y = lambda f -> z + (y * f)',
                          'Decl(bar, Lambda([z, y], Lambda([f], (z + (y * f)))))')

    def test_basic_app(self):
        self.assertParsed('foo x = gob(x)',
                          'Decl(foo, Lambda([x], App(gob, [x])))')
        self.assertParsed('foo x = bob(x, true)',
                          'Decl(foo, Lambda([x], App(bob, [x, true])))')
        self.assertParsed('foo x = bob(x, max(10), true)',
                          'Decl(foo, Lambda([x], App(bob, [x, App(max, [10]), true])))')

    def test_full_exprs(self):
        self.assertParsed(
            'bar = if ((t + p) * v) > 0 then x else f(y)',
            'Decl(bar, If((((t + p) * v) > 0), x, App(f, [y])))')
        self.assertParsed(
            'bar = joe(moe(doe(false)))',
            'Decl(bar, App(joe, [App(moe, [App(doe, [false])])]))')
        self.assertParsed(
            'cake = lambda f -> lambda x -> f(3) - f(x)',
            'Decl(cake, Lambda([f], Lambda([x], (App(f, [3]) - App(f, [x])))))')
        self.assertParsed(
            'cake = lambda f x -> f(3) - f(x)',
            'Decl(cake, Lambda([f, x], (App(f, [3]) - App(f, [x]))))')


if __name__ == '__main__':
    unittest.main()
