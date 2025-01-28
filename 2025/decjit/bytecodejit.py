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


def emit_exprcode(func):
    bc = func.__code__
    print(f"co_varnames = {bc.co_varnames}")
    print(f"co_argcount = {bc.co_argcount}")
    for inst in dis.get_instructions(func):
        print(inst)

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

        expr = emit_exprcode(func)
        print(expr)

        # emitter = ExprCodeEmitter()
        # emitter.visit(tree)

        # cg = LLVMCodeGenerator()
        # cg.codegen(emitter.return_expr, len(emitter.args))

        # return llvm_jit_evaluate(emitter.return_expr, *args)

    return wrapper
