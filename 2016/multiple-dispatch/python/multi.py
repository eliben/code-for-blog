# Tested with Python 3.4
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

class _MultiMethod:
    def __init__(self, name):
        self.name = name
        self.typemap = {}
    def __call__(self, *args):
        types = tuple(arg.__class__ for arg in args)
        function = self.typemap.get(types)
        if function is None:
            raise TypeError("no match")
        return function(*args)
    def register_function_for_types(self, types, function):
        if types in self.typemap:
            raise TypeError("duplicate registration")
        self.typemap[types] = function


_multi_registry = {}

def multimethod(*types):
    def register(function):
        name = function.__name__
        mm = _multi_registry.get(name)
        if mm is None:
            mm = _multi_registry[name] = _MultiMethod(name)
        mm.register_function_for_types(types, function)
        return mm
    return register


class Shape:
    @property
    def name(self):
        return self.__class__

class Rectangle(Shape): pass

class Ellipse(Shape): pass

class Triangle(Shape): pass


@multimethod(Rectangle, Ellipse)
def intersect(r, e):
    print('Rectangle x Ellipse [names r=%s, e=%s]' % (r.name, e.name))

@multimethod(Rectangle, Rectangle)
def intersect(r1, r2):
    print('Rectangle x Rectangle [names r1=%s, r2=%s]' % (r1.name, r2.name))

@multimethod(Rectangle, Shape)
def intersect(r, s):
    print('Rectangle x Shape [names r=%s, s=%s]' % (r.name, s.name))


if __name__ == '__main__':
    r1 = Rectangle()
    r2 = Rectangle()
    e = Ellipse()

    intersect(r1, e)
    intersect(r1, r2)
