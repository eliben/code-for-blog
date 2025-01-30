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


Box.__add__ = Box.__radd__ = _add


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
