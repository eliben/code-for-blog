import functools
import dis


class BytecodeJITError(Exception):
    pass


def bytecodejit(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        if kwargs:
            raise BytecodeJITError("Keyword arguments are not supported")

        bc = func.__code__
        print(f"co_varnames = {bc.co_varnames}")
        print(f"co_argcount = {bc.co_argcount}")
        for inst in dis.get_instructions(func):
            print(inst)

        # emitter = ExprCodeEmitter()
        # emitter.visit(tree)

        # cg = LLVMCodeGenerator()
        # cg.codegen(emitter.return_expr, len(emitter.args))

        # return llvm_jit_evaluate(emitter.return_expr, *args)

    return wrapper
