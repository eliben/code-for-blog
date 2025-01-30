# Tracing based JIT decorator.
#
# Eli Bendersky (https://eli.thegreenplace.net)
# This code is in the public domain
import functools
import inspect
from dataclasses import dataclass

from exprcode import (
    Expr,
    VarExpr,
    ConstantExpr,
    BinOpExpr,
    Op,
    llvm_jit_evaluate,
)


class TraceJITError(Exception):
    pass


@dataclass
class _Box:
    expr: Expr


def _register_binary_op(opcode, reverse=False):
    """Registers a binary opcode for Boxes.

    If reverse is True, the operation is registered as arg2 <op> arg1,
    instead of arg1 <op> arg2.
    """

    def _op(arg1, arg2):
        if reverse:
            arg1, arg2 = arg2, arg1
        box1 = arg1 if isinstance(arg1, _Box) else _Box(ConstantExpr(arg1))
        box2 = arg2 if isinstance(arg2, _Box) else _Box(ConstantExpr(arg2))
        # Note: theoretically we could do constant folding here, if the only
        # arguments are constants.
        return _Box(BinOpExpr(box1.expr, box2.expr, opcode))

    return _op


_Box.__add__ = _Box.__radd__ = _register_binary_op(Op.ADD)
_Box.__sub__ = _register_binary_op(Op.SUB)
_Box.__rsub__ = _register_binary_op(Op.SUB, reverse=True)
_Box.__mul__ = _Box.__rmul__ = _register_binary_op(Op.MUL)
_Box.__truediv__ = _register_binary_op(Op.DIV)
_Box.__rtruediv__ = _register_binary_op(Op.DIV, reverse=True)


def tracejit(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        if kwargs:
            raise TraceJITError("Keyword arguments are not supported")

        argspec = inspect.getfullargspec(func)

        argboxes = []
        for i, arg in enumerate(args):
            if i >= len(argspec.args):
                raise TraceJITError("Too many arguments")
            argboxes.append(_Box(VarExpr(argspec.args[i], i)))

        out_box = func(*argboxes)
        return llvm_jit_evaluate(out_box.expr, *args)

    return wrapper
