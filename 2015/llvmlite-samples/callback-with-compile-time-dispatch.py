# llvmlite sample - emit the address of a Python function into binary using
# llvmlite, and call it.
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
import ctypes
from ctypes import c_int64, c_void_p
import sys

import llvmlite.ir as ir
import llvmlite.binding as llvm

cb_func_ty = ir.FunctionType(ir.IntType(64),
                             [ir.IntType(64), ir.IntType(64)])
cb_func_ptr_ty = cb_func_ty.as_pointer()
i64_ty = ir.IntType(64)

def create_addrcaller(m, addr):
    # define i64 @addrcaller(i64 %a, i64 %b) #0 {
    # entry:
    #   %f = inttoptr i64% ADDR to i64 (i64, i64)*
    #   %call = tail call i64 %f(i64 %a, i64 %b)
    #   ret i64 %call
    # }
    addrcaller_func_ty = ir.FunctionType(i64_ty, [i64_ty, i64_ty])
    addrcaller_func = ir.Function(m, addrcaller_func_ty, name='addrcaller')
    a = addrcaller_func.args[0]; a.name = 'a'
    b = addrcaller_func.args[1]; b.name = 'b'
    irbuilder = ir.IRBuilder(addrcaller_func.append_basic_block('entry'))
    f = irbuilder.inttoptr(ir.Constant(i64_ty, addr),
                           cb_func_ptr_ty, name='f')
    call = irbuilder.call(f, [a, b])
    irbuilder.ret(call)

def main():
    CBFUNCTY = ctypes.CFUNCTYPE(c_int64, c_int64, c_int64)
    def myfunc(a, b):
        print('I was called with {0} and {1}'.format(a, b))
        return a + b
    cb_myfunc = CBFUNCTY(myfunc)
    cb_addr = ctypes.cast(cb_myfunc, c_void_p).value
    print('Callback address is 0x{0:x}'.format(cb_addr))

    module = ir.Module()
    create_addrcaller(module, cb_addr)
    print(module)

    llvm.initialize()
    llvm.initialize_native_target()
    llvm.initialize_native_asmprinter()

    llvm_module = llvm.parse_assembly(str(module))

    tm = llvm.Target.from_default_triple().create_target_machine()

    # Compile the module to machine code using MCJIT
    with llvm.create_mcjit_compiler(llvm_module, tm) as ee:
        ee.finalize_object()
        # Now call addrcaller
        print('Calling "addrcaller"')
        addrcaller = ctypes.CFUNCTYPE(c_int64, c_int64, c_int64)(
            ee.get_function_address("addrcaller"))
        print(addrcaller)
        res = addrcaller(105, 23)
        print('  The result is', res)

if __name__ == '__main__':
    main()