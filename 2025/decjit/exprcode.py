from dataclasses import dataclass
from enum import Enum

class Node:
    pass

@dataclass
class ConstantNode(Node):
    value: float

@dataclass
class VarNode(Node):
    name: str


class Op(Enum):
    ADD = '+'
    SUB = '-'
    MUL = '*'
    DIV = '/'

@dataclass
class BinOpNode(Node):
    left: Node
    right: Node
    op: Op
