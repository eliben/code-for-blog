# A simple recursive descent parser that implements an integer
# calculator. This parser suffers from an associativity problem
# due to using BNF-y recursion for rules like <term> ad <expr>
#
# BNF:
#
# <stmt>    : set <id> = <expr>
#           | <expr>
# <expr>    : <term> + <expr>
#           | <term> - <expr>
#           | <term>
# <term>    : <factor> * <term>
#           | <factor> / <term>
#           | <factor>
# <factor>  : <id>
#           | <number>
#           | ( <expr> )
#
# <id>      : [a-zA-Z_]\w+
# <number>  : \d+
#
# This grammar is LL(1), suitable for predictive parsing.
#
#-----------------------------------------------
# Eli Bendersky (eliben@gmail.com)
# License: this code is in the public domain
# Last modified: March 2009
#-----------------------------------------------
#
try:
    import eblib.lexer as lexer
except ImportError:
    import lexer


class ParseError(Exception): pass


class CalcParser(object):
    def __init__(self):
        lex_rules = [
            ('set',             'SET'),
            ('\d+',             'NUMBER'),
            ('[a-zA-Z_]\w*',    'IDENTIFIER'),
            ('\+',              '+'),
            ('\-',              '-'),
            ('\*',              '*'),
            ('\/',              '/'),
            ('\(',              '('),
            ('\)',              ')'),
            ('=',               '='),
        ]

        self.lexer = lexer.Lexer(lex_rules, skip_whitespace=True)
        self._clear()

    def parse(self, line):
        """ Parse a new line of input and return its result.

            Variables defined in previous calls to parse can be
            used in following ones.

            ParseError can be raised in case of errors.
        """
        self.lexer.input(line)
        self._get_next_token()
        return self._stmt()

    def _clear(self):
        self.cur_token = None
        self.var_table = {}

    def _error(self, msg):
        raise ParseError(msg)

    def _get_next_token(self):
        try:
            self.cur_token = self.lexer.token()

            if self.cur_token is None:
                self.cur_token = lexer.Token(None, None, None)
        except lexer.LexerError, e:
            self._error('Lexer error at position %d' % e.pos)

    def _match(self, type):
        """ The 'match' primitive of RD parsers.

            * Verifies that the current token is of the given type
            * Returns the value of the current token
            * Reads in the next token
        """
        if self.cur_token.type == type:
            val = self.cur_token.val
            self._get_next_token()
            return val
        else:
            self._error('Unmatched %s' % type)

    # The toplevel rule of the parser.
    #
    # <stmt>    : set <id> = <expr>
    #           | <expr>
    #
    def _stmt(self):
        if self.cur_token.type is None:
            return ''
        elif self.cur_token.type == 'SET':
            self._match('SET')
            id_name = self._match('IDENTIFIER')
            self._match('=')
            expr_val = self._expr()

            self.var_table[id_name] = expr_val
            return expr_val
        else:
            return self._expr()

    # <expr>    : <term> + <expr>
    #           | <term> - <expr>
    #           | <term>
    #
    def _expr(self):
        lval = self._term()

        if self.cur_token.type == '+':
            self._match('+')
            op = lambda a, b: a + b
        elif self.cur_token.type == '-':
            self._match('-')
            op = lambda a, b: a - b
        else:
            print 'returning lval = %s' % lval
            return lval

        rval = self._expr()
        print 'lval = %s, rval = %s, res = %s' % (
            lval, rval, op(lval, rval))
        return op(lval, rval)

    # <term>    : <factor> * <term>
    #           | <factor> / <term>
    #           | <factor>
    #
    def _term(self):
        lval = self._factor()

        if self.cur_token.type == '*':
            self._match('*')
            op = lambda a, b: a * b
        elif self.cur_token.type == '/':
            self._match('/')
            op = lambda a, b: a / b
        else:
            return lval

        rval = self._term()
        return op(lval, rval)

    # <factor>  : <id>
    #           | <number>
    #           | ( <expr> )
    #
    def _factor(self):
        if self.cur_token.type == '(':
            self._match('(')
            val = self._expr()
            self._match(')')
            return val
        elif self.cur_token.type == 'NUMBER':
            return int(self._match('NUMBER'))
        elif self.cur_token.type == 'IDENTIFIER':
            id_name = self._match('IDENTIFIER')

            try:
                val = self.var_table[id_name]
            except KeyError:
                self._error('Unknown identifier `%s`' % id_name)
            return val
        else:
            self._error('Invalid factor `%s`' % self.cur_token.val)


if __name__ == '__main__':
    p = CalcParser()
    print p.parse('5 - 1 - 2')
    print p.parse('set x = 5')
    print p.parse('set y = 2 * x')
    print p.parse('(5+y)*3 + 3')
