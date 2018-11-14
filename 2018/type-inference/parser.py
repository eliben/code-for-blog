# EBNF specification for micro-ML. { x } means zero or more repetitions of x.
#
# The top-level is decl.
#
# decl:                   ID { ID } '=' expr
#
# expr:                   INT
#                       | bool
#                       | ID
#                       | ID '(' { expr ',' } ')'
#                       | '(' expr ')'
#                       | expr op expr
#                       | 'if' expr 'then' expr 'else' expr
#                       | 'lambda' { ID } '->' expr
#
# op:                   + | * | - | == | > | >= | <= | < | !=
# bool:                 'true' | 'false'
#
# ID: identifier
# INT: an integer
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import ast
import lexer


class ParseError(Exception):
    pass


class Parser:
    """Parser for micro-ML.

    The only public method here is parse_decl that parses a 'decl' from a
    string. Usage:

      p = Parser()
      decl = p.parse_decl(<some micro-ML code>)
      # decl is now an ast.Decl node

    parse_decl() can be called multiple times with the same parser to parse
    multiple decls (state is wiped out between calls).
    """
    def __init__(self):
        lex_rules = (
            ('if',              'IF'),
            ('then',            'THEN'),
            ('else',            'ELSE'),
            ('true',            'TRUE'),
            ('false',           'FALSE'),
            ('lambda',          'LAMBDA'),
            ('\d+',             'INT'),
            ('->',              'ARROW'),
            ('!=',              '!='),
            ('==',              '=='),
            ('>=',              '>='),
            ('<=',              '<='),
            ('<',               '<'),
            ('>',               '>'),
            ('\+',              '+'),
            ('\-',              '-'),
            ('\*',              '*'),
            ('\(',              '('),
            ('\)',              ')'),
            ('=',               '='),
            (',',               ','),
            ('[a-zA-Z_]\w*',    'ID'),
        )
        self.lexer = lexer.Lexer(lex_rules, skip_whitespace=True)
        self.cur_token = None
        self.operators = {'!=', '==', '>=', '<=', '<', '>', '+', '-', '*'}

    def parse_decl(self, text):
        """Parse declaration given in text and return an AST node for it."""
        self.lexer.input(text)
        self._get_next_token()
        decl = self._decl()
        if self.cur_token.type != None:
            self._error('Unexpected token "{}" (at #{})'.format(
                self.cur_token.val, self.cur_token.pos))
        return decl

    def _error(self, msg):
        raise ParseError(msg)

    def _get_next_token(self):
        """Advances the parser's internal lexer to the next token.

        This method doesn't return anything; it assigns self.cur_token to the
        next token in the input stream.
        """
        try:
            self.cur_token = self.lexer.token()

            if self.cur_token is None:
                self.cur_token = lexer.Token(None, None, None)
        except lexer.LexerError as e:
            self._error('Lexer error at position {}: {}'.format(e.pos, e))

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
            self._error('Unmatched {} (found {})'.format(type,
                                                         self.cur_token.type))

    def _decl(self):
        name = self._match('ID')
        argnames = []

        # If we have arguments, collect them. Only IDs allowed here.
        while self.cur_token.type == 'ID':
            argnames.append(self.cur_token.val)
            self._get_next_token()

        self._match('=')
        expr = self._expr()
        if len(argnames) > 0:
            return ast.Decl(name, ast.LambdaExpr(argnames, expr))
        else:
            return ast.Decl(name, expr)

    def _expr(self):
        """Parse an expr of the form:

                expr op expr

           We only allow a single operator between expressions. Additional
           operators should be nested using parens, e.g. x + (y * z)
        """
        node = self._expr_component()
        if self.cur_token.type in self.operators:
            op = self.cur_token.type
            self._get_next_token()
            rhs = self._expr_component()
            return ast.OpExpr(op, node, rhs)
        else:
            return node

    def _expr_component(self):
        """Parse an expr component (components can be separated by an operator).
        """
        curtok = self.cur_token
        if self.cur_token.type == 'INT':
            self._get_next_token()
            return ast.IntConstant(curtok.val)
        elif self.cur_token.type in ('FALSE', 'TRUE'):
            self._get_next_token()
            return ast.BoolConstant(curtok.val)
        elif self.cur_token.type == 'ID':
            self._get_next_token()
            if self.cur_token.type == '(':
                # ID followed by '(' is function application
                return self._app(curtok.val)
            else:
                return ast.Identifier(curtok.val)
        elif self.cur_token.type == '(':
            self._get_next_token()
            expr = self._expr()
            self._match(')')
            return expr
        elif self.cur_token.type == 'IF':
            return self._ifexpr()
        elif self.cur_token.type == 'LAMBDA':
            return self._lambda()
        else:
            self._error("Don't support {} yet".format(curtok.type))

    def _ifexpr(self):
        self._match('IF')
        ifexpr = self._expr()
        self._match('THEN')
        thenexpr = self._expr()
        self._match('ELSE')
        elseexpr = self._expr()
        return ast.IfExpr(ifexpr, thenexpr, elseexpr)

    def _lambda(self):
        self._match('LAMBDA')
        argnames = []

        while self.cur_token.type == 'ID':
            argnames.append(self.cur_token.val)
            self._get_next_token()

        if len(argnames) < 1:
            self._error('Expected non-empty argument list for lambda')
        self._match('ARROW')
        expr = self._expr()
        return ast.LambdaExpr(argnames, expr)

    def _app(self, name):
        self._match('(')
        args = []
        while self.cur_token.type != ')':
            args.append(self._expr())
            if self.cur_token.type == ',':
                self._get_next_token()
            elif self.cur_token.type == ')':
                pass # the loop will break
            else:
                self._error("Unexpected {} in application".format(
                    self.cur_token.val))
        self._match(')')
        return ast.AppExpr(ast.Identifier(name), args)
