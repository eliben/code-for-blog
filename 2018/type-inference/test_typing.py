# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import unittest

import parser
from typing import *


class TestAssignTypenames(unittest.TestCase):
    def test_decl(self):
        p = parser.Parser()
        e = p.parse_decl('foo f x = f(3) - f(x)')
        assign_typenames(e.expr)
        self.assertEqual(str(e.expr.expr.left.func._type), 't1')
        self.assertEqual(str(e.expr.expr.right.func._type), 't1')
        self.assertEqual(str(e.expr.expr.right.args[0]._type), 't2')

        self.assertEqual(str(e.expr.expr._type), 't3')
        self.assertEqual(str(e.expr.expr.left._type), 't4')
        self.assertEqual(str(e.expr.expr.right._type), 't5')


class TestGenerateEquations(unittest.TestCase):
    def has_equation(self, eqs, left, right):
        """Assert that the list of equations eqs has a left=right equation.

        left and right are given in string representations.
        """
        for e in eqs:
            if str(e.left) == left and str(e.right) == right:
                return True
        return False

    def setUp(self):
        reset_type_counter()

    def test_decl(self):
        p = parser.Parser()
        e = p.parse_decl('foo f x = f(3) - f(x)')
        equations = []
        assign_typenames(e.expr)
        generate_equations(e.expr, equations)
        self.assertTrue(self.has_equation(equations, 't1', '(Int -> t4)'))
        self.assertTrue(self.has_equation(equations, 't1', '(t2 -> t5)'))


class TestFullInference(unittest.TestCase):
    def setTyp(self):
        reset_type_counter()

    def assertInferredType(self, decl, ty):
        """Assert that the type of the declaration is inferred to ty."""
        p = parser.Parser()
        e = p.parse_decl(decl)
        assign_typenames(e.expr)
        equations = []
        generate_equations(e.expr, equations)
        unifier = unify_all_equations(equations)
        inferred = get_expression_type(e.expr, unifier, rename_types=True)
        self.assertEqual(str(inferred), ty)

    def test_simple(self):
        self.assertInferredType('foo = 9', 'Int')
        self.assertInferredType('foo = false', 'Bool')
        self.assertInferredType('foo x = 9', '(a -> Int)')
        self.assertInferredType('foo x = true', '(a -> Bool)')
        self.assertInferredType('foo x y = x + y', '((Int, Int) -> Int)')
        self.assertInferredType('foo x y = x == y', '((Int, Int) -> Bool)')

    def test_if(self):
        self.assertInferredType(
            'foo x y = if x > y then 10 else 20',
            '((Int, Int) -> Int)')
        self.assertInferredType(
            'foo x = if x then 10 else 20',
            '(Bool -> Int)')
        self.assertInferredType(
            'foo x = if x then x else x',
            '(Bool -> Bool)')

    def test_full(self):
        self.assertInferredType(
            'foo f = f(11)',
            '((Int -> a) -> a)')
        self.assertInferredType(
            'foo g h = g(h(0))',
            '(((b -> a), (Int -> b)) -> a)')
        self.assertInferredType(
            'foo f g x = f(g(3 + x))',
            '(((b -> a), (Int -> b), Int) -> a)')
        self.assertInferredType(
            'foo f x = f(3) - f(x)',
            '(((Int -> Int), Int) -> Int)')
        self.assertInferredType(
            'foo f g x = if f(x) then g(x) else 20',
            '(((a -> Bool), (a -> Int), a) -> Int)')
        self.assertInferredType(
            'foo f g x = if f(x == 1) then g(x) else 20',
            '(((Bool -> Bool), (Int -> Int), Int) -> Int)')
        self.assertInferredType(
            'foo f = lambda t -> f(t)',
            '((b -> a) -> (b -> a))')
        self.assertInferredType(
            'foo f x = if x then lambda t -> f(t) else lambda j -> f(x)',
            '(((Bool -> a), Bool) -> (Bool -> a))')


if __name__ == '__main__':
    unittest.main()
