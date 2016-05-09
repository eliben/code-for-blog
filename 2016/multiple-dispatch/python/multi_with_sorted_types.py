# Variation of multi.py supporting type-sorting on multimethod dispatch.
# This lets invocations be symmetric.
#
# Tested with Python 3.4
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

#
# This part is the multimethod "library".
#

class _MultiMethod:
    """Maps tuples of argument types to function to call for these types."""
    def __init__(self, name):
        self.name = name
        self.typemap = {}

    def __call__(self, *args):
        # Find the right function to call based on a sorted tuple of types. We
        # have to sort the call arguments themselves together with the types,
        # so that the handler function can get them in the order it expects.
        args_with_types = sorted(
            zip(args, (arg.__class__ for arg in args)),
            key=lambda pair: id(pair[1]))
        types = tuple(ty for _, ty in args_with_types)
        try:
            args = (arg for arg, _ in args_with_types)
            return self.typemap[types](*args)
        except KeyError:
            raise TypeError('no match %s for types %s' % (self.name, types))

    def register_function_for_types(self, types, function):
        # Sort the tuple of types before setting it in the dispatch map.
        types = tuple(sorted(types, key=id))
        if types in self.typemap:
            raise TypeError("duplicate registration")
        self.typemap[types] = function


# Maps function.__name__ -> _MultiMethod object.
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

#
# From here on an example of client code: using multimethods for dispatching
# shape intersections.
#
class Shape:
    @property
    def name(self):
        return self.__class__

class Rectangle(Shape): pass

class Ellipse(Shape): pass

class Triangle(Shape): pass


# Will handle (Rectangle, Ellipse) but also (Ellipse, Rectangle). The recrangle
# will always be passed as the first argument.
# Note that this assumes that Rectangle < Ellipse (so that the sorting order
# will place Rectangle first).
@multimethod(Rectangle, Ellipse)
def intersect(r, e):
    print('Rectangle x Ellipse [names r=%s, e=%s]' % (r.name, e.name))

@multimethod(Rectangle, Rectangle)
def intersect(r1, r2):
    print('Rectangle x Rectangle [names r1=%s, r2=%s]' % (r1.name, r2.name))

@multimethod(Shape, Shape)
def intersect(s1, s2):
    print('Shape x Shape [names s1=%s, s2=%s]' % (s1.name, s2.name))


if __name__ == '__main__':
    r = Rectangle()
    e = Ellipse()

    intersect(r, e)
    intersect(e, r)
