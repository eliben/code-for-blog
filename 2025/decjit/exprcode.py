# A simple expression based "IR" with support for emitting LLVM IR and JIT.
#
# Expressions are trees of Expr nodes.
#
# Eli Bendersky (https://eli.thegreenplace.net)
# This code is in the public domain
from dataclasses import dataclass
from enum import Enum

import llvmlite.ir as ir
import llvmlite.binding as llvm

from ctypes import CFUNCTYPE, c_double


class Expr:
    pass


@dataclass
class ConstantExpr(Expr):
    value: float


# VarExpr is a variable reference. It has a name and an optional argument index.
# The argument index is used to reference positional function arguments.
@dataclass
class VarExpr(Expr):
    """Variable reference.

    arg_idx is the index of the positional argument in the expression evaluation
    call.
    name is optional - for debugging."""

    name: str
    arg_idx: int


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


class _LLVMCodeGenerator:
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
        return self.module

    def _codegen_expr(self, expr):
        match expr:
            case ConstantExpr(value):
                return ir.Constant(ir.DoubleType(), value)
            case VarExpr(_, arg_idx):
                if arg_idx >= len(self.args):
                    raise CodegenError(f"Invalid argument index {arg_idx}")
                return self.args[arg_idx]
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


def llvm_jit_evaluate(expr: Expr, *args: float) -> float:
    """Use LLVM JIT to evaluate the given expression with *args.

    expr is an instance of Expr. *args are the arguments to the expression, each
    a float. The arguments must match the arguments the expression expects.

    Returns the result of evaluating the expression.
    """
    llvm.initialize()
    llvm.initialize_native_target()
    llvm.initialize_native_asmprinter()
    llvm.initialize_native_asmparser()

    cg = _LLVMCodeGenerator()
    modref = llvm.parse_assembly(str(cg.codegen(expr, len(args))))

    target = llvm.Target.from_default_triple()
    target_machine = target.create_target_machine()
    with llvm.create_mcjit_compiler(modref, target_machine) as ee:
        ee.finalize_object()
        cfptr = ee.get_function_address("func")
        cfunc = CFUNCTYPE(c_double, *([c_double] * len(args)))(cfptr)
        return cfunc(*args)
