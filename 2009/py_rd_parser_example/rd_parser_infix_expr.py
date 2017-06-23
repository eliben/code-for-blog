# A recursive descent parser that implements an integer calculator
# with variables and conditional statements.
#
# This parser implements exactly the same grammar as
# rd_parser_ebnf, but it evaluates expressions using a different
# technique. Instead of recursively evaluating them following the
# EBNF grammar, it uses an embedded infix expression evaluator
# based on the Shunting Yard algorithm.
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
    #               | <infix_expr>
    #
    def _stmt(self):
        if self.cur_token.type is None:
            return ''
        elif self.cur_token.type == 'SET':
            return self._assign_stmt()
        elif self.cur_token.type == 'IF':
            return self._if_stmt()
        else:
            return self._infix_eval()

    # <if_stmt>     : if <infix_expr> then <stmt> [else <stmt>]
    #
    def _if_stmt(self):
        self._match('IF')
        condition = self._infix_eval()
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

    # <assign_stmt> : set <id> = <infix_expr>
    #
    def _assign_stmt(self):
        self._match('SET')
        id_name = self._match('IDENTIFIER')
        self._match('=')
        expr_val = self._infix_eval()

        # When syntax checking, don't actually do the assignment
        #
        if not self.only_syntax_check:
            self.var_table[id_name] = expr_val

        return expr_val

    ##
    ## The infix expression evaluator.
    ## Returns the value of the evaluated expression.
    ##
    ## Infix expressions are numbers and identifiers separated by
    ## binary (and unary) operators, possibly with parts delimited
    ## by parentheses. The operators supported by this evaluator
    ## and their precedences are controlled through the _ops
    ## table.
    ##
    ## Internally, uses two stacks. One for keeping the operations
    ## that still await results, and another for keeping the
    ## results.
    ##
    ##

    def _infix_eval(self):
        """ Run the infix evaluator and return the result.
        """
        self.op_stack = []
        self.res_stack = []

        self.op_stack.append(self._sentinel)
        self._infix_eval_expr()
        return self.res_stack[-1]

    class Op(object):
        """ Represents an operator recognized by the infix
            evaluator. Each operator has a numeric precedence,
            and flags specifying whether it's unary/binary and
            right/left associative.
        """
        def __init__(   self, name, op, prec,
                        unary=False, right_assoc=False):
            self.name = name
            self.op = op
            self.prec = prec
            self.unary = unary
            self.binary = not self.unary
            self.right_assoc = right_assoc
            self.left_assoc = not self.right_assoc

        def apply(self, *args):
            return self.op(*args)

        def precedes(self, other):
            """ The '>' operator from the Shunting Yard algorithm.
                I don't call it '>' on purpose, as its semantics
                are unusual (i.e. this is not the familiar
                algebraic '>')
            """
            if self.binary and other.binary:
                if self.prec > other.prec:
                    return True
                elif self.left_assoc and (self.prec == other.prec):
                    return True
            elif self.unary and other.binary:
                return self.prec >= other.prec

            return False

        def __repr__(self):
            return '<%s(%s)>' % (self.name, self.prec)

    # The operators recognized by the evaluator.
    #
    _ops = {
        'u-':   Op('unary -', operator.neg, 90, unary=True),
        '**':   Op('**', operator.pow, 70, right_assoc=True),
        '*':    Op('*', operator.mul, 50),
        '/':    Op('/', operator.div, 50),
        '+':    Op('+', operator.add, 40),
        '-':    Op('-', operator.sub, 40),
        '<<':   Op('<<', operator.lshift, 35),
        '>>':   Op('>>', operator.rshift, 35),
        '&':    Op('&', operator.and_, 30),
        '^':    Op('^', operator.xor, 29),
        '|':    Op('|', operator.or_, 28),
        '>':    Op('>', operator.gt, 20),
        '>=':   Op('>=', operator.ge, 20),
        '<':    Op('<', operator.lt, 20),
        '<=':   Op('<=', operator.le, 20),
        '==':   Op('==', operator.eq, 15),
        '!=':   Op('!=', operator.ne, 15),
    }

    # A set of operators that can be unary. If such an operator
    # is found, 'u' is prepended to its symbol for finding it in
    # the _ops table
    #
    _unaries = set(['-'])

    # Dummy operator with the lowest possible precedence (the
    # Sentinel value in the Shunting Yard algorithm)
    #
    _sentinel = Op(None, None, 0)

    def _infix_eval_expr(self):
        """ Evaluates an 'expression' - atoms separated by binary
            operators.
        """
        self._infix_eval_atom()

        while ( self.cur_token.type in self._ops and
                self._ops[self.cur_token.type].binary):
            self._push_op(self._ops[self.cur_token.type])
            self._get_next_token()
            self._infix_eval_atom()

        while self.op_stack[-1] != self._sentinel:
            self._pop_op()

    def _infix_eval_atom(self):
        """ Evaluates an 'atom' - either an identifier/number, or
            an atom prefixed by a unary operation, or a full
            expression inside parentheses.
        """
        if self.cur_token.type in ['IDENTIFIER', 'NUMBER']:
            self.res_stack.append(self._compute_val(self.cur_token))
            self._get_next_token()
        elif self.cur_token.type == '(':
            self._get_next_token()
            self.op_stack.append(self._sentinel)
            self._infix_eval_expr()
            self._match(')')
            self.op_stack.pop()
        elif self.cur_token.type in self._unaries:
            self._push_op(self._ops['u' + self.cur_token.type])
            self._get_next_token()
            self._infix_eval_atom()

    def _push_op(self, op):
        """ Pushes an operation onto the op stack.
            But first computes and removes all higher-precedence
            operators from it.
        """
        #~ print 'push_op: stack =', self.op_stack
        #~ print '    ...', op
        while self.op_stack[-1].precedes(op):
            self._pop_op()
        self.op_stack.append(op)
        #~ print '    ... =>', self.op_stack

    def _pop_op(self):
        """ Pops an operation from the op stack, computing its
            result and storing it on the result stack.
        """
        #~ print 'pop_op: op_stack =', self.op_stack
        #~ print '    ... res_stack =', self.res_stack
        top_op = self.op_stack.pop()

        if top_op.unary:
            self.res_stack.append(top_op.apply(self.res_stack.pop()))
        else:
            if len(self.res_stack) < 2:
                self._error('Not enough arguments for operator %s' % top_op.name)

            t1 = self.res_stack.pop()
            t0 = self.res_stack.pop()
            self.res_stack.append(top_op.apply(t0, t1))
        #~ print '    ... => res_stack =', self.res_stack

    def _compute_val(self, tok):
        """ Compute the value of a number or an identifier.
        """
        if tok.type == 'NUMBER':
            return int(tok.val)
        elif tok.type == 'IDENTIFIER':
            if self.only_syntax_check:
                return 0
            else:
                try:
                    val = self.var_table[tok.val]
                except KeyError:
                    self._error('Unknown identifier `%s`' % tok.val)
                return val
        else:
            assert 0


if __name__ == '__main__':
    p = CalcParser()

    print p.calc('2 + 4 - (8 + 5) * 3 ** 2 - 1')
    #~ p.calc('set p = 1')
    #~ p.calc('set p = p * 2')
    #~ p.calc('if 5 > 5 then set p = p * 2 else set p = 0')
