# Python 3.6
import lexer

class Expr:
    pass


# In App, function names are always considered to be constants, not variables.
# This simplifies things and doesn't affect expressivity. We can always model
# variable functions by envisioning an apply(FUNCNAME, ... args ...).
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
        return '$' + self.name

    def __eq__(self, other):
        return type(self) == type(other) and self.name == other.name


class Const(Expr):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return self.value

    def __eq__(self, other):
        return type(self) == type(other) and self.value == other.value


class ParseError(Exception): pass


def parse_expr(s):
    """Parses an expression from string s, returns an Expr."""
    parser = ExprParser(s)
    return parser.parse_expr()


class ExprParser:
    """Expression parser.

    Use the top-level parse_expr() instead of instantiating this class directly.
    """
    def __init__(self, text):
        self.text = text
        self.cur_token = None
        lexrules = (
            ('\d+',             'NUMBER'),
            ('[a-zA-Z_]\w*',    'ID'),
            (',',               'COMMA'),
            ('\(',              'LP'),
            ('\)',              'RP'),
        )
        self.lexer = lexer.Lexer(lexrules, skip_whitespace=True)
        self.lexer.input(text)
        self._get_next_token()

    def _get_next_token(self):
        try:
            self.cur_token = self.lexer.token()

            if self.cur_token is None:
                self.cur_token = lexer.Token(None, None, None)
        except lexer.LexerError as e:
            self._error('Lexer error at position %d' % e.pos)

    def _error(self, msg):
        raise ParseError(msg)

    def parse_expr(self):
        if self.cur_token.type == 'NUMBER':
            expr = Const(self.cur_token.val)
            # Consume the current token and return the Const expr.
            self._get_next_token()
            return expr
        elif self.cur_token.type == 'ID':
            # We have to look at the next token to distinguish between App and
            # Var.
            idtok = self.cur_token
            self._get_next_token()
            if self.cur_token.type == 'LP':
                if idtok.val.isupper():
                    self._error("Function names should be constant")
                self._get_next_token()
                args = []
                while True:
                    args.append(self.parse_expr())
                    if self.cur_token.type == 'RP':
                        break
                    elif self.cur_token.type == 'COMMA':
                        # Consume the comma and continue to the next arg
                        self._get_next_token()
                    else:
                        self._error("Expected ',' or ')' in application")
                # Consume the ')'
                self._get_next_token()
                return App(fname=idtok.val, args=args)
            else:
                if idtok.val.isupper():
                    return Var(idtok.val)
                else:
                    return Const(idtok.val)


def occurs_check(v, expr, bindings):
    """Does the variable v occur anywhere inside expr?

    Variables in expr are looked up in bindings and the check is applied
    recursively.
    """
    assert isinstance(v, Var)
    if v == expr:
        return True
    elif isinstance(expr, Var) and expr.name in bindings:
        return occurs_check(v, bindings[expr.name], bindings)
    elif isinstance(expr, App):
        return any(occurs_check(v, arg, bindings) for arg in expr.args)
    else:
        return False


def unify(x, y, bindings):
    """Unifies expressions x and y with initial bindings.

    Returns a bindings (map of name->Expr) that unifies x and y, or None if
    they can't be unified. Pass bindings={} if no bindings are initially
    known. Note that {} means valid (but empty) bindings.
    """
    if bindings is None:
        return None
    elif x == y:
        return bindings
    elif isinstance(x, Var):
        return unify_variable(x, y, bindings)
    elif isinstance(y, Var):
        return unify_variable(y, x, bindings)
    elif isinstance(x, App) and isinstance(y, App):
        if x.fname != y.fname or len(x.args) != len(y.args):
            return None
        else:
            newbindings = bindings.copy()
            for i in len(x.args):
                newbindings = unify(x.args[i], y.args[i], newbindings)
            return newbindings
    else:
        return None


def unify_variable(v, x, bindings):
    """Unifies variable v with expression x, using bindings.

    Returns updated bindings or None on failure.
    """
    assert isinstance(v, Var)
    if v.name in bindings:
        return unify(bindings[v.name], x, bindings)
    elif isinstance(x, Var) and x.name in bindings:
        return unify(v, bindings[x.name], bindings)
    elif occurs_check(v, x, bindings):
        return None
    else:
        # v is not yet in bindings and can't simplify x. Extend bindings.
        bindings[v.name] = x
        return bindings


# TODO: Need a bindings map to pass around for unify


if __name__ == '__main__':
    s = 'f(g(h(X)))'
    ep = ExprParser(s)
    expr = ep.parse_expr()
    print(expr)
