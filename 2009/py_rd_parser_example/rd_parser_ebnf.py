# A recursive descent parser that implements an integer calculator
# with variables and conditional statements.
# The grammar is LL(1), suitable for predictive parsing.
#
# EBNF:
#
# <stmt>        : <assign_stmt>
#               | <if_stmt>
#               | <cmp_expr>
#
# <assign_stmt> : set <id> = <cmp_expr>
#
## Note 'else' binds to the innermost 'if', like in C
#
# <if_stmt>     : if <cmp_expr> then <stmt> [else <stmt>]
#
# <cmp_expr>    : <bitor_expr> [== <bitor_expr>]
#               | <bitor_expr> [!= <bitor_expr>]
#               | <bitor_expr> [> <bitor_expr>]
#               | <bitor_expr> [< <bitor_expr>]
#               | <bitor_expr> [>= <bitor_expr>]
#               | <bitor_expr> [<= <bitor_expr>]
#
# <bitor_expr>  | <bitxor_expr> {| <bitxor_expr>}
#
# <bitxor_expr> | <bitand_expr> {^ <bitand_expr>}
#
# <bitand_expr> | <shift_expr> {& <shift_expr>}
#
# <shift_expr>  | <arith_expr> {<< <arith_expr>}
#               : <arith_expr> {>> <arith_expr>}
#
# <arith_expr>  : <term> {+ <term>}
#               | <term> {- <term>}
#
# <term>        : <power> {* <power>}
#               | <power> {/ <power>}
#
# <power>       : <factor> ** <power>
#               | <factor>
#
# <factor>      : <id>
#               | <number>
#               | - <factor>
#               | ( <cmp_expr> )
#
# <id>          : [a-zA-Z_]\w+
# <number>      : \d+
#
# Employs EBNF and looping to solve the associativity problem in
# <term> and <arith_expr>.
# Note that <power> is defined recursively and not using EBNF
# grouping {** <factor>}. This is on purpose - as it makes the
# right-associativity of exponentation naturally expressed in
# the recursion.
#
#-----------------------------------------------
# Eli Bendersky (eliben@gmail.com)
# License: this code is in the public domain
# Last modified: March 2009
#-----------------------------------------------
#
from __future__ import with_statement
from contextlib import contextmanager
import operator

try:
    import eblib.lexer as lexer
except ImportError:
    import lexer


class ParseError(Exception): pass


class CalcParser(object):
    """ The calculator statement parser. Evaluates statements
        and expressions on the fly, returning a numeric result
        for all calc() calls.
    """
    def __init__(self):
        lex_rules = [
            ('set',             'SET'),
            ('if',              'IF'),
            ('then',            'THEN'),
            ('else',            'ELSE'),
            ('\d+',             'NUMBER'),
            ('[a-zA-Z_]\w*',    'IDENTIFIER'),
            ('\*\*',            '**'),
            ('!=',              '!='),
            ('==',              '=='),
            ('>=',              '>='),
            ('<=',              '<='),
            ('>>',              '>>'),
            ('<<',              '<<'),
            ('&',               '&'),
            ('\^',              '^'),
            ('\|',              '|'),
            ('<',               '<'),
            ('>',               '>'),
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

    def calc(self, line):
        """ Parse a new line of input and return its result.

            Variables defined in previous calls to calc can be
            used in following ones.

            ParseError can be raised in case of errors.
        """
        self.lexer.input(line)
        self._get_next_token()

        val = self._stmt()

        if self.cur_token.type != None:
            self._error('Unexpected token %s (at #%s)' % (
                self.cur_token.val, self.cur_token.pos))

        return val

    def _clear(self):
        self.cur_token = None
        self.var_table = {}
        self.only_syntax_check = False

    # Some rules are parsed with the self.only_syntax_check flag
    # turned on. This means that the syntactic structure of the
    # rules has to be checked, but no side effects are to be
    # executed. Example side effect: assignment to a variable.
    #
    # This is used, for example, when a branch of an if statement
    # is not taken (e.g. the 'else' branch of a true condition),
    # but we should still verify that the syntax is correct.
    #
    # To implement this, the syntax_check context manager can be
    # used. When a rule wants to parse some sub-rule with
    # self.only_syntax_check turned on, it can do it as follows:
    #
    # with self._syntax_check():
    #    ... parse sub-rules
    #
    # This will ensure that the only_syntax_check flag is set
    # before the sub-rules are parsed and turned off after.
    #
    @contextmanager
    def _syntax_check(self):
        # We must catch and reraise exceptions (for example,
        # ParseError can happen), but turn off the flag anyway,
        # so that subsequent statements won't be affected.
        #
        try:
            self.only_syntax_check = True
            yield
        except:
            raise
        finally:
            self.only_syntax_check = False

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
            self._error('Unmatched %s (found %s)' % (
                type, self.cur_token.type))

    # The toplevel rule of the parser.
    #
    # <stmt>        : <assign_stmt>
    #               | <if_stmt>
    #               | <cmp_expr>
    #
    def _stmt(self):
        if self.cur_token.type is None:
            return ''
        elif self.cur_token.type == 'SET':
            return self._assign_stmt()
        elif self.cur_token.type == 'IF':
            return self._if_stmt()
        else:
            return self._cmp_expr()

    # <if_stmt>     : if <cmd_expr> then <stmt> [else <stmt>]
    #
    def _if_stmt(self):
        self._match('IF')
        condition = self._cmp_expr()
        self._match('THEN')

        if condition:
            # The condition is true, so we'll evaluate the 'then'
            # clause, and only syntax check the 'else' clause,
            # if there is one.
            #
            result = self._stmt()

            if self.cur_token.type == 'ELSE':
                self._match('ELSE')
                with self._syntax_check():
                    self._stmt()

            return result
        else:
            # The condition is false, so we'll only syntax check
            # the 'then' clause, and evaluate the 'else' clause,
            # if there is one.
            #
            with self._syntax_check():
                self._stmt()

            if self.cur_token.type == 'ELSE':
                self._match('ELSE')
                return self._stmt()
            else:
                return None

    # <assign_stmt> : set <id> = <cmp_expr>
    #
    def _assign_stmt(self):
        self._match('SET')
        id_name = self._match('IDENTIFIER')
        self._match('=')
        expr_val = self._cmp_expr()

        # When syntax checking, don't actually do the assignment
        #
        if not self.only_syntax_check:
            self.var_table[id_name] = expr_val

        return expr_val

    # <cmp_expr>    : <bitor_expr> [== <bitor_expr>]
    #               | <bitor_expr> [!= <bitor_expr>]
    #               | <bitor_expr> [> <bitor_expr>]
    #               | <bitor_expr> [< <bitor_expr>]
    #               | <bitor_expr> [>= <bitor_expr>]
    #               | <bitor_expr> [<= <bitor_expr>]
    #
    _cmp_op_map = {
        '==':   operator.eq,
        '!=':   operator.ne,
        '>=':   operator.ge,
        '>':    operator.gt,
        '<=':   operator.le,
        '<':    operator.lt,
    }

    def _cmp_expr(self):
        lval = self._bitor_expr()

        for op_name, op in self._cmp_op_map.iteritems():
            if self.cur_token.type == op_name:
                self._match(op_name)
                return apply(op, [lval, self._bitor_expr()])

        # No known comparison op matched...
        #
        return lval

    # <bitor_expr>  | <bitxor_expr> {| <bitxor_expr>}
    #
    def _bitor_expr(self):
        lval = self._bitxor_expr()

        while self.cur_token.type == '|':
            self._match('|')
            lval |= self._bitxor_expr()

        return lval

    # <bitxor_expr> | <bitand_expr> {^ <bitand_expr>}
    #
    def _bitxor_expr(self):
        lval = self._bitand_expr()

        while self.cur_token.type == '^':
            self._match('^')
            lval ^= self._bitand_expr()

        return lval

    # <bitand_expr> | <shift_expr> {& <shift_expr>}
    #
    def _bitand_expr(self):
        lval = self._shift_expr()

        while self.cur_token.type == '&':
            self._match('&')
            lval &= self._shift_expr()

        return lval

    # <shift_expr>  | <arith_expr> {<< <arith_expr>}
    #               : <arith_expr> {>> <arith_expr>}
    #
    def _shift_expr(self):
        lval = self._arith_expr()

        while self.cur_token.type in ['>>', '<<']:
            if self.cur_token.type == '>>':
                self._match('>>')
                lval >>= self._arith_expr()
            elif self.cur_token.type == '<<':
                self._match('<<')
                lval <<= self._arith_expr()

        return lval

    # <arith_expr>  : <term> {+ <term>}
    #               | <term> {- <term>}
    #
    def _arith_expr(self):
        lval = self._term()

        while self.cur_token.type in ['+', '-']:
            if self.cur_token.type == '+':
                self._match('+')
                lval += self._term()
            elif self.cur_token.type == '-':
                self._match('-')
                lval -= self._term()

        return lval

    # <term>    : <power> {* <power>}
    #           | <power> {/ <power>}
    #
    def _term(self):
        lval = self._power()

        while self.cur_token.type in ['/', '*']:
            if self.cur_token.type == '*':
                self._match('*')
                lval *= self._power()
            elif self.cur_token.type == '/':
                self._match('/')
                lval /= self._power()

        return lval

    # <power>   : <factor> ** <power>
    #           | <factor>
    #
    def _power(self):
        lval = self._factor()

        if self.cur_token.type == '**':
            self._match('**')
            lval **= self._power()

        return lval

    # <factor>  : <id>
    #           | <number>
    #           | - <factor>
    #           | ( <cmp_expr> )
    #
    def _factor(self):
        if self.cur_token.type == '(':
            self._match('(')
            val = self._cmp_expr()
            self._match(')')
            return val
        elif self.cur_token.type == 'NUMBER':
            return int(self._match('NUMBER'))
        elif self.cur_token.type == '-':
            self._match('-')
            return -(self._factor())
        elif self.cur_token.type == 'IDENTIFIER':
            id_name = self._match('IDENTIFIER')

            # When syntax checking, we don't care if the variable
            # was defined prior to use
            #
            if self.only_syntax_check:
                return 0
            else:
                try:
                    val = self.var_table[id_name]
                except KeyError:
                    self._error('Unknown identifier `%s`' % id_name)
                return val
        else:
            self._error('Invalid factor `%s`' % self.cur_token.val)


def calculator_prompt():
    """ A toy calculator prompt for interactive computations.
    """
    print 'Welcome to the calculator. Press Ctrl+C to exit.'
    cp = CalcParser()

    try:
        while True:
            try:
                line = raw_input('--> ')
                print cp.calc(line)
            except ParseError, err:
                print 'Error:', err

    except KeyboardInterrupt:
        print '... Thanks for using the calculator.'


if __name__ == '__main__':
    import sys

    if len(sys.argv) > 1 and sys.argv[1] == '-p':
        calculator_prompt()
        sys.exit()

    p = CalcParser()

    #
    # If stuff works correctly, this will print 42
    #

    p.calc('set joe = 4 - 5 - 1')               # 0
    print p.calc('joe')

    #~ p.calc('set mar = joe + 2 ** 4 * -3')       # -48
    #~ p.calc('set pie = 2 ** 3 ** 2')             # 512
    #~ p.calc('if joe != 0 then set pie = 3')      # pie stays 512
    #~ p.calc('if 1 == 1 then set k = 10 else set k = 20') # 10
    #~ p.calc('if k > 20 then set k = 12')         # k stays 10
    #~ p.calc('if k <= 11 then set t = 0 else set t = 2') # 0
    #~ print p.calc('pie - (k * -mar) + k + t')    # 42
