# Sample of using typing.TypeVar with covariant settings.
# Run with python3.6+
#
# For type-checking with mypy:
#
# > mypy covar-typecheck.py
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
from typing import List, TypeVar, Iterable, Generic

class Mammal:
    pass

class Cat(Mammal):
    pass

def count_mammals_list(seq : List[Mammal]) -> int:
    return len(seq)

mlst = [Mammal(), Mammal()]
print(count_mammals_list(mlst))

# This will fail a mypy check, because List is not covariant.
clst = [Cat(), Cat()]
print(count_mammals_list(clst))

# Now we define a simplistic immutable list wrapper with a covariant type
# parameter. This will pass type checking
T_co = TypeVar('T_co', covariant=True)

class ImmutableList(Generic[T_co]):
    def __init__(self, items: Iterable[T_co]) -> None:
        self.lst = list(items)

    def __len__(self) -> int:
        return len(self.lst)

def count_mammals_ilist(seq : ImmutableList[Mammal]) -> int:
    return len(seq)

mimmlst = ImmutableList([Mammal(), Mammal()])
print(count_mammals_ilist(mimmlst))

cimmlst = ImmutableList([Cat(), Cat()])
print(count_mammals_ilist(cimmlst))
