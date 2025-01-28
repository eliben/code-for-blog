"Decorator JITs - Python as a DSL" ?

These implement the different kinds of JIT:

    astjit/
    bytecodejit/
    tracejit/

All using the same package for running expressions via llvmlite:

    exprcode/

To run unit tests:

    uv run pytest -v
