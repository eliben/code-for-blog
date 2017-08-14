# Folds in Python.
#
# Tested with Python 3.4+ (though I expect it to work with 2.x too).
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
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


@TraceCalls(show_ret=True)
def foldr(func, init, seq):
    if not seq:
        return init
    else:
        return func(seq[0], foldr(func, init, seq[1:]))


@TraceCalls(show_ret=True)
def foldl(func, init, seq):
    if not seq:
        return init
    else:
        return foldl(func, func(init, seq[0]), seq[1:])


@TraceCalls(show_ret=True)
def product_reducer(seqval, acc):
    return seqval * acc


@TraceCalls()
def product_with_foldr(seq):
    return foldr(product_reducer, 1, seq)


@TraceCalls()
def product_with_foldl(seq):
    return foldl(product_reducer, 1, seq)


@TraceCalls()
def ratio_with_foldl(seq):
    return foldl(lambda acc, seqval: acc / seqval, 1, seq)


@TraceCalls(show_ret=True)
def multiply(a, b):
    return a * b


identity = lambda x: x


@TraceCalls(show_ret=True)
def productl_with_foldr(seq):
    return foldr(
                lambda seqval, acc: lambda n: acc(multiply(n, seqval)),
                identity,
                seq)(1)


print(productl_with_foldr([2, 4, 6, 8]))
