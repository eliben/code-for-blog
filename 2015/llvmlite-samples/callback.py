# llvmlite sample - pass the address of a Python function to a JITed function
# that calls it back.
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
from ctypes import c_int64, c_void_p, CFUNCTYPE
import sys

import llvmlite.ir as ir
import llvmlite.binding as llvm

def create_caller(m):
    # define i64 @caller(i64 (i64, i64)* nocapture %f, i64 %i) #0 {
    # entry:
    #   %mul = shl nsw i64 %i, 1
    #   %call = tail call i64 %f(i64 %i, i64 %mul) #1
    #   ret i64 %call
    # }
    i64_ty = ir.IntType(64)

    # The callback function 'caller' accepts is a pointer to FunctionType with
    # the appropriate signature.
    cb_func_ptr_ty = ir.FunctionType(i64_ty, [i64_ty, i64_ty]).as_pointer()
    caller_func_ty = ir.FunctionType(i64_ty, [cb_func_ptr_ty, i64_ty])

    caller_func = ir.Function(m, caller_func_ty, name='caller')
    caller_func.args[0].name = 'f'
    caller_func.args[1].name = 'i'
    irbuilder = ir.IRBuilder(caller_func.append_basic_block('entry'))
    mul = irbuilder.mul(caller_func.args[1],
                        ir.Constant(i64_ty, 2),
                        name='mul')
    call = irbuilder.call(caller_func.args[0], [caller_func.args[1], mul])
    irbuilder.ret(call)

def myfunc(a, b):
    print('I was called with {0} and {1}'.format(a, b))
    return a + b

def main():
    module = ir.Module()
    create_caller(module)

    llvm.initialize()
    llvm.initialize_native_target()
    llvm.initialize_native_asmprinter()

    llvm_module = llvm.parse_assembly(str(module))
    tm = llvm.Target.from_default_triple().create_target_machine()

    # Compile the module to machine code using MCJIT.
    with llvm.create_mcjit_compiler(llvm_module, tm) as ee:
        ee.finalize_object()

        # Obtain a pointer to the compiled 'caller' - it's the address of its
        # JITed code in memory.
        CBFUNCTY = CFUNCTYPE(c_int64, c_int64, c_int64)
        cfptr = ee.get_function_address("caller")
        callerfunc = CFUNCTYPE(c_int64, CBFUNCTY, c_int64)(cfptr)

        # Wrap myfunc in CBFUNCTY and pass it as a callback to caller.
        cb_myfunc = CBFUNCTY(myfunc)
        print('Calling "caller"')
        res = callerfunc(cb_myfunc, 42)
        print('  The result is', res)

if __name__ == '__main__':
    main()