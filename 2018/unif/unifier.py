# Python 3.6

class Expr:
    pass


class App(Expr):
    def __init__(self, fname, args=()):
       self.fname = fname
       self.args = args

    def __str__(self):
        return '{0}({1})'.format(self.fname, ','.join(map(str, self.args)))


class Var(Expr):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name


class Const(Expr):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return self.value
