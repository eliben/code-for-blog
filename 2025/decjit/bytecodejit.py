# Bytecode-analysis based JIT decorator.
#
# Eli Bendersky (https://eli.thegreenplace.net)
# This code is in the public domain
import functools
import dis

from exprcode import (
    VarExpr,
    ConstantExpr,
    BinOpExpr,
    Op,
    llvm_jit_evaluate,
)


class BytecodeJITError(Exception):
    pass


def _emit_exprcode(func):
    bc = func.__code__
    stack = []
    for inst in dis.get_instructions(func):
        match inst.opname:
            case "LOAD_FAST":
                idx = inst.arg
                stack.append(VarExpr(bc.co_varnames[idx], idx))
            case "LOAD_CONST":
                stack.append(ConstantExpr(inst.argval))
            case "BINARY_OP":
                right = stack.pop()
                left = stack.pop()
                match inst.argrepr:
                    case "+":
                        stack.append(BinOpExpr(left, right, Op.ADD))
                    case "-":
                        stack.append(BinOpExpr(left, right, Op.SUB))
                    case "*":
                        stack.append(BinOpExpr(left, right, Op.MUL))
                    case "/":
                        stack.append(BinOpExpr(left, right, Op.DIV))
                    case _:
                        raise BytecodeJITError(f"Unsupported operator {inst.argval}")
            case "RETURN_VALUE":
                if len(stack) != 1:
                    raise BytecodeJITError("Invalid stack state")
                return stack.pop()
            case "RESUME" | "CACHE":
                # Skip nops
                pass
            case _:
                raise BytecodeJITError(f"Unsupported opcode {inst.opname}")


def bytecodejit(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        if kwargs:
            raise BytecodeJITError("Keyword arguments are not supported")

        expr = _emit_exprcode(func)
        return llvm_jit_evaluate(expr, *args)

    return wrapper
