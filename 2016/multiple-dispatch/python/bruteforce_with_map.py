# A variation of the "brute-force" solution using a map dispatching on types.
#
# Tested with Python 3.4
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

class Shape:
    @property
    def name(self):
        return self.__class__

class Rectangle(Shape): pass

class Ellipse(Shape): pass

class Triangle(Shape): pass

def intersect_rectangle_ellipse(r, e):
    print('Rectangle x Ellipse [names r=%s, e=%s]' % (r.name, e.name))

def intersect_rectangle_rectangle(r1, r2):
    print('Rectangle x Rectangle [names r1=%s, r2=%s]' % (r1.name, r2.name))

def intersect_generic(s1, s2):
    print('Shape x Shape [names s1=%s, s2=%s]' % (s1.name, s2.name))

_dispatch_map = {
    (Rectangle, Ellipse): intersect_rectangle_ellipse,
    (Rectangle, Rectangle): intersect_rectangle_rectangle,
}

def intersect(s1, s2):
    handler = _dispatch_map.get((type(s1), type(s2)), intersect_generic)
    handler(s1, s2)

if __name__ == '__main__':
    r1 = Rectangle()
    r2 = Rectangle()
    e = Ellipse()
    t = Triangle()

    intersect(r1, e)
    intersect(r1, r2)
    intersect(t, e)
