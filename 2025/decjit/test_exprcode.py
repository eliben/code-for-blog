from exprcode import (
    VarExpr,
    ConstantExpr,
    BinOpExpr,
    Op,
    llvm_jit_evaluate,
)


def test_codegen_expr():
    expr = BinOpExpr(
        left=VarExpr("x", 0),
        right=BinOpExpr(
            left=ConstantExpr(2.0),
            right=VarExpr("y", 1),
            op=Op.MUL,
        ),
        op=Op.ADD,
    )
    result = llvm_jit_evaluate(expr, 3.0, 4.0)
    assert result == 11.0
