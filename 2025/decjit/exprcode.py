from dataclasses import dataclass
from enum import Enum

import llvmlite.ir as ir
import llvmlite.binding as llvm


class Expr:
    pass


@dataclass
class ConstantExpr(Expr):
    value: float


# VarExpr is a variable reference. It has a name and an optional argument index.
# The argument index is used to reference positional function arguments.
@dataclass
class VarExpr(Expr):
    name: str
    arg_idx: int = None


class Op(Enum):
    ADD = "+"
    SUB = "-"
    MUL = "*"
    DIV = "/"


@dataclass
class BinOpExpr(Expr):
    left: Expr
    right: Expr
    op: Op


class CodegenError(Exception):
    pass


class LLVMCodeGenerator:
    def __init__(self):
        self.module = ir.Module()
        self.builder = None
        self.args = []

    def codegen(self, expr, num_args):
        fty = ir.FunctionType(ir.DoubleType(), [ir.DoubleType()] * num_args)
        func = ir.Function(self.module, fty, name="func")
        for i, arg in enumerate(func.args):
            self.args.append(arg)
        bbentry = func.append_basic_block(name="entry")
        self.builder = ir.IRBuilder(bbentry)
        retval = self._codegen_expr(expr)
        self.builder.ret(retval)

    def _codegen_expr(self, expr):
        match expr:
            case ConstantExpr(value):
                return ir.Constant(ir.DoubleType(), value)
            case VarExpr(name, arg_idx):
                if arg_idx is not None:
                    return self.args[arg_idx]
                else:
                    raise CodegenError(f"Unknown variable {name}")
            case BinOpExpr(left, right, op):
                lval = self._codegen_expr(left)
                rval = self._codegen_expr(right)
                match op:
                    case Op.ADD:
                        return self.builder.fadd(lval, rval)
                    case Op.SUB:
                        return self.builder.fsub(lval, rval)
                    case Op.MUL:
                        return self.builder.fmul(lval, rval)
                    case Op.DIV:
                        return self.builder.fdiv(lval, rval)
                    case _:
                        raise CodegenError(f"Unsupported operator {op}")
            case _:
                raise CodegenError(f"Unsupported expression {expr}")
