# AST-analysis based JIT decorator.
#
# Eli Bendersky (https://eli.thegreenplace.net)
# This code is in the public domain
import ast
import functools
import inspect

from exprcode import (
    VarExpr,
    ConstantExpr,
    BinOpExpr,
    Op,
    llvm_jit_evaluate,
)


class ASTJITError(Exception):
    pass


class _ExprCodeEmitter(ast.NodeVisitor):
    def __init__(self):
        self.args = []
        self.return_expr = None
        self.op_map = {
            ast.Add: Op.ADD,
            ast.Sub: Op.SUB,
            ast.Mult: Op.MUL,
            ast.Div: Op.DIV,
        }

    def visit_FunctionDef(self, node):
        self.args = [arg.arg for arg in node.args.args]
        if len(node.body) != 1 or not isinstance(node.body[0], ast.Return):
            raise ASTJITError("Function must consist of a single return statement")
        self.visit(node.body[0])

    def visit_Return(self, node):
        self.return_expr = self.visit(node.value)

    def visit_Name(self, node):
        try:
            idx = self.args.index(node.id)
        except ValueError:
            raise ASTJITError(f"Unknown variable {node.id}")
        return VarExpr(node.id, idx)

    def visit_Constant(self, node):
        return ConstantExpr(node.value)

    def visit_BinOp(self, node):
        left = self.visit(node.left)
        right = self.visit(node.right)
        try:
            op = self.op_map[type(node.op)]
            return BinOpExpr(left, right, op)
        except KeyError:
            raise ASTJITError(f"Unsupported operator {node.op}")


def astjit(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        if kwargs:
            raise ASTJITError("Keyword arguments are not supported")
        source = inspect.getsource(func)
        tree = ast.parse(source)

        emitter = _ExprCodeEmitter()
        emitter.visit(tree)
        return llvm_jit_evaluate(emitter.return_expr, *args)

    return wrapper
