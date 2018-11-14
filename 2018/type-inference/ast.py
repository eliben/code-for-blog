# AST nodes for micro-ML.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

class ASTNode:
    def visit_children(self, func):
        """Visit all children with a function that takes a child node."""
        for child in self._children:
            func(child)

    # Used by the type inference algorithm.
    _type = None

    # Used by passes that traverse the AST. Each concrete node class lists the
    # sub-nodes it has as children.
    _children = []


class IntConstant(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)


class BoolConstant(ASTNode):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)


class Identifier(ASTNode):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name


class OpExpr(ASTNode):
    """Binary operation between expressions."""
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right
        self._children = [self.left, self.right]

    def __str__(self):
        return '({} {} {})'.format(self.left, self.op, self.right)


class AppExpr(ASTNode):
    """Application of a function to a sequence of arguments.

    func is a node, args is a sequence of nodes.
    """
    def __init__(self, func, args=()):
        self.func = func
        self.args = args
        self._children = [self.func, *self.args]

    def __str__(self):
        return 'App({}, [{}])'.format(self.func,
                                      ', '.join(map(str, self.args)))


class IfExpr(ASTNode):
    """if ... then ... else ... expression."""
    def __init__(self, ifexpr, thenexpr, elseexpr):
       self.ifexpr = ifexpr
       self.thenexpr = thenexpr
       self.elseexpr = elseexpr
       self._children = [self.ifexpr, self.thenexpr, self.elseexpr]

    def __str__(self):
        return 'If({}, {}, {})'.format(
            self.ifexpr, self.thenexpr, self.elseexpr)


class LambdaExpr(ASTNode):
    """lambda [args] -> expr"""
    def __init__(self, argnames, expr):
        self.argnames = argnames
        self.expr = expr
        self._children = [self.expr]

    def __str__(self):
        return 'Lambda([{}], {})'.format(', '.join(self.argnames), self.expr)

    # Used by the type inference algorithm to map discovered types for the
    # arguments of the lambda. Since we list arguments as names (strings) and
    # not ASTNodes, we can't keep their _type on the node.
    _arg_types = None


class Decl(ASTNode):
    """Declaration mapping name = expr.

    For functions expr is a Lambda node.
    """
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr
        self._children = [self.expr]

    def __str__(self):
        return 'Decl({}, {})'.format(
            self.name, self.expr)
