These implement the different kinds of JIT:

    astjit
    bytecodejit
    tracejit

Each has its own `sample-<kind>jit.py` file for experiments. Run these with
`uv run`.

All using the same package for running expressions via llvmlite:

    exprcode

To run all unit tests:

    uv run pytest -v
