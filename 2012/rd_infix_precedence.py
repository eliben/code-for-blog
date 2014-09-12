#-----------------------------------------------
# Precedence climbing expression parser.
#
# Eli Bendersky (eliben@gmail.com)
# License: this code is in the public domain
# Last modified: July 2012
#-----------------------------------------------
from collections import namedtuple
import re


Tok = namedtuple('Tok', 'name value')


class Tokenizer(object):
    """ Simple tokenizer object. The cur_token attribute holds the current
        token (Tok). Call get_next_token() to advance to the
        next token. cur_token is None before the first token is
        taken and after the source ends.
    """
    TOKPATTERN = re.compile("\s*(?:(\d+)|(.))")

    def __init__(self, source):
        self._tokgen = self._gen_tokens(source)
        self.cur_token = None

    def get_next_token(self):
        """ Advance to the next token, and return it.
        """
        try:
            self.cur_token = self._tokgen.next()
        except StopIteration:
            self.cur_token = None
        return self.cur_token

    def _gen_tokens(self, source):
        for number, operator in self.TOKPATTERN.findall(source):
            if number:
                yield Tok('NUMBER', number)
            elif operator == '(':
                yield Tok('LEFTPAREN', '(')
            elif operator == ')':
                yield Tok('RIGHTPAREN', ')')
            else:
                yield Tok('BINOP', operator)

    def __repr__(self):
        return 'Tokenizer(cur_token=%s)' % str(self.cur_token)



# For each operator, a (precedence, associativity) pair.
OpInfo = namedtuple('OpInfo', 'prec assoc')

OPINFO_MAP = {
    '+':    OpInfo(1, 'LEFT'),
    '-':    OpInfo(1, 'LEFT'),
    '*':    OpInfo(2, 'LEFT'),
    '/':    OpInfo(2, 'LEFT'),
    '^':    OpInfo(3, 'RIGHT'),
}


def parse_error(msg):
    raise RuntimeError(msg)


from eblib.tracer import TraceCalls

@TraceCalls(show_ret=True)
def compute_atom(tokenizer):
    tok = tokenizer.cur_token
    if tok.name == 'LEFTPAREN':
        tokenizer.get_next_token()
        val = compute_expr(tokenizer, 1)
        if tokenizer.cur_token.name != 'RIGHTPAREN':
            parse_error('unmatched "("')
        tokenizer.get_next_token()
        return val
    elif tok is None:
            parse_error('source ended unexpectedly')
    elif tok.name == 'BINOP':
        parse_error('expected an atom, not an operator "%s"' % tok.value)
    else:
        assert tok.name == 'NUMBER'
        tokenizer.get_next_token()
        return int(tok.value)


@TraceCalls(show_ret=True)
def compute_expr(tokenizer, min_prec):
    atom_lhs = compute_atom(tokenizer)

    while True:
        cur = tokenizer.cur_token
        if (cur is None or cur.name != 'BINOP'
                        or OPINFO_MAP[cur.value].prec < min_prec):
            break

        # Inside this loop the current token is a binary operator
        assert cur.name == 'BINOP'

        # Get the operator's precedence and associativity, and compute a
        # minimal precedence for the recursive call
        op = cur.value
        prec, assoc = OPINFO_MAP[op]
        next_min_prec = prec + 1 if assoc == 'LEFT' else prec

        # Consume the current token and prepare the next one for the
        # recursive call
        tokenizer.get_next_token()
        atom_rhs = compute_expr(tokenizer, next_min_prec)

        # Update lhs with the new value
        atom_lhs = compute_op(op, atom_lhs, atom_rhs)

    return atom_lhs


def compute_op(op, lhs, rhs):
    lhs = int(lhs); rhs = int(rhs)
    if op == '+':   return lhs + rhs
    elif op == '-': return lhs - rhs
    elif op == '*': return lhs * rhs
    elif op == '/': return lhs / rhs
    elif op == '^': return lhs ** rhs
    else:
        parse_error('unknown operator "%s"' % op)


def test():
    def compute(s):
        t = Tokenizer(s)
        t.get_next_token()
        return compute_expr(t, 1)

    assert compute('1 + 2 * 3') == 7
    assert compute('7 - 9 * (2 - 3)') == 16
    assert compute('2 * 3 * 4') == 24
    assert compute('2 ^ 3 ^ 4') == 2 ** (3 ** 4)
    assert compute('(2 ^ 3) ^ 4') == 4096
    assert compute('5') == 5
    assert compute('4 + 2') == 6
    assert compute('9 - 8 - 7') == -6
    assert compute('9 - (8 - 7)') == 8
    assert compute('(9 - 8) - 7') == -6
    assert compute('2 + 3 ^ 2 * 3 + 4') == 33


if __name__ == '__main__':
    #test()

    t = Tokenizer('2 + 3^2*3 + 4')
    t.get_next_token()
    print compute_expr(t, min_prec=1)

