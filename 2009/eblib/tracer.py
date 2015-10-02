#-------------------------------------------------------------------------------
# tracer.py
#
# A decorator for tracing function calls.
# Runs with both Python 2 and 3.
#
# Loosely based on:
#  - http://paulbutler.org/archives/python-debugging-with-decorators/
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
# Last modified: August 2012
#-------------------------------------------------------------------------------
import sys
from functools import wraps

class TraceCalls(object):
    """ Use as a decorator on functions that should be traced. Several
        functions can be decorated - they will all be indented according
        to their call depth.
    """
    def __init__(self, stream=sys.stdout, indent_step=2, show_ret=False):
        self.stream = stream
        self.indent_step = indent_step
        self.show_ret = show_ret

        # This is a class attribute since we want to share the indentation
        # level between different traced functions, in case they call
        # each other.
        TraceCalls.cur_indent = 0

    def __call__(self, fn):
        @wraps(fn)
        def wrapper(*args, **kwargs):
            indent = ' ' * TraceCalls.cur_indent
            argstr = ', '.join(
                [repr(a) for a in args] +
                ["%s=%s" % (a, repr(b)) for a, b in kwargs.items()])
            self.stream.write('%s%s(%s)\n' % (indent, fn.__name__, argstr))

            TraceCalls.cur_indent += self.indent_step
            ret = fn(*args, **kwargs)
            TraceCalls.cur_indent -= self.indent_step

            if self.show_ret:
                self.stream.write('%s--> %s\n' % (indent, ret))
            return ret
        return wrapper


#------------------------------------------------------------------------------
if __name__ == '__main__':
    @TraceCalls()
    def iseven(n):
        return True if n == 0 else isodd(n - 1)

    @TraceCalls()
    def isodd(n):
        return False if n == 0 else iseven(n - 1)

    print(iseven(7))

    @TraceCalls(indent_step=4, show_ret=True)
    def flatten(lst):
        if isinstance(lst, list):
            return sum((flatten(item) for item in lst), [])
        else:
            return [lst]

    list(flatten([1, 2, [3, [4, 5], 6, [7, [9], 12]], 4, [6, 9]]))


