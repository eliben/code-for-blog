import ast
import functools
import inspect

from exprcode import VarExpr


class ExprCodeEmitter(ast.NodeVisitor):
    def __init__(self):
        self.args = []
        self.return_expr = None

    def visit_FunctionDef(self, node):
        self.args = [arg.arg for arg in node.args.args]
        self.visit(node.body[0])
        # todo verify the body has a single return

    def visit_Return(self, node):
        self.return_expr = self.visit(node.value)
    
    def visit_Name(self, node):
        return VarExpr(node.id)

    def Visit_Constant(self, node):
        return node.value


def astjit(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        source = inspect.getsource(func)
        tree = ast.parse(source)
        print(ast.dump(tree, indent=4))
        return func(*args, **kwargs)
    return wrapper
