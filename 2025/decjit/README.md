"Decorator JITs - Python as a DSL" ?

These implement the different kinds of JIT:

    astjit/
    bytecodejit/
    tracejit/

All using the same package for running expressions via llvmlite:

    exprcode/

The root dir will have the sample scripts we can just run. Unit tests?
If files are named `*_test.py`, pytest will discover them.

