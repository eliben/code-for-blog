from exprcode import *


# Build up an expression from Expr nodes
expr = BinOpExpr(
    left=VarExpr("x", 0),
    right=BinOpExpr(
        left=ConstantExpr(2.0),
        right=VarExpr("y", 1),
        op=Op.MUL,
    ),
    op=Op.ADD,
)

def test_codegen_expr():
    result = llvm_jit_evaluate(expr, 3.0, 4.0)
    assert result == 11.0