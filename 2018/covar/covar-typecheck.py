from typing import TypeVar, List

class Mammal:
    pass

class Cat(Mammal):
    pass

T = TypeVar('T')

def count_mammals(seq : List[Mammal]) -> int:
    return len(seq)

lst = [1, 2, 3]
mlst = [Mammal(), Mammal()]
clst = [Cat(), Cat()]

print(count_mammals(clst))
