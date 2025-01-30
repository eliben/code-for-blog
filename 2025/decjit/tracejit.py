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
class Box:
    expr: Expr


def _add(*args):
    boxes = []
    for arg in args:
        if isinstance(arg, Box):
            boxes.append(arg)
        else:
            boxes.append(Box(ConstantExpr(arg)))

    # Note: theoretically we could do constant folding here, if the only
    # arguments are constants.

    return Box(BinOpExpr(boxes[0].expr, boxes[1].expr, Op.ADD))


def register_binary_op(opcode, reverse=False):
    """Registers a binary opcode for Boxes.

    If reverse is True, the operation is registered as arg2-arg1, instead of
    arg1-arg2.
    """

    def _op(arg1, arg2):
        if reverse:
            arg1, arg2 = arg2, arg1
        box1 = arg1 if isinstance(arg1, Box) else Box(ConstantExpr(arg1))
        box2 = arg2 if isinstance(arg2, Box) else Box(ConstantExpr(arg2))
        # Note: theoretically we could do constant folding here, if the only
        # arguments are constants.
        return Box(BinOpExpr(box1.expr, box2.expr, opcode))

    return _op


Box.__add__ = Box.__radd__ = register_binary_op(Op.ADD)
Box.__sub__ = register_binary_op(Op.SUB)
Box.__rsub__ = register_binary_op(Op.SUB, reverse=True)
Box.__mul__ = Box.__rmul__ = register_binary_op(Op.MUL)
Box.__truediv__ = register_binary_op(Op.DIV)
Box.__rtruediv__ = register_binary_op(Op.DIV, reverse=True)


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
            argboxes.append(Box(VarExpr(argspec.args[i], i)))

        expr = func(*argboxes)
        print(f"expr = {expr}")
        # return llvm_jit_evaluate(emitter.return_expr, *args)

    return wrapper
