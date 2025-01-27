from dataclasses import dataclass
from enum import Enum

class Expr:
    pass

@dataclass
class ConstantExpr(Expr):
    value: float

@dataclass
class VarExpr(Expr):
    name: str


class Op(Enum):
    ADD = '+'
    SUB = '-'
    MUL = '*'
    DIV = '/'

@dataclass
class BinOpExpr(Expr):
    left: Expr
    right: Expr
    op: Op
