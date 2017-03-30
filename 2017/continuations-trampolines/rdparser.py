# Recursive-descent parser.
#
# Grammar:
#
# <expr>    : <term> + <expr>
#             <term>
# <term>    : <factor> * <factor>
#             <factor>
# <factor>  : <number>
#           | '(' <expr> ')'
# <number>  : \d+
#
# Note: our implementation here is simplistic, and suffers from the
# associativity problem described in
# http://eli.thegreenplace.net/2009/03/14/some-problems-of-recursive-descent-parsers/
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

def parse_expr(tokens):
    lval = parse_term(tokens)
    if len(tokens) == 0:
        return lval
    op_tok = tokens[0]
    if op_tok == '+':
        op = lambda a, b: a + b
    else:
        return lval
    tokens.pop(0)
    rval = parse_expr(tokens)
    return op(lval, rval)

def parse_term(tokens):
    lval = parse_factor(tokens)
    if len(tokens) == 0:
        return lval
    op_tok = tokens[0]
    if op_tok == '*':
        op = lambda a, b: a * b
    else:
        return lval
    tokens.pop(0)
    rval = parse_term(tokens)
    return op(lval, rval)

def parse_factor(tokens):
    tok = tokens.pop(0)
    try:
        toknum = int(tok)
        return toknum
    except ValueError:
        pass
    if tok == '(':
        exprval = parse_expr(tokens)
    if tokens[0] != ')':
        raise RuntimeError('expected matching ")", got', tokens[0])
    tokens.pop(0)
    return exprval


e = '2 + 3 * ( 5 + 4 * 2 )'
etoks = e.split()
print('parsing:', etoks)
print(parse_expr(etoks))


def test_parser(parserfunc):
    assert parserfunc('2'.split()) == 2
    assert parserfunc('2 * 4'.split()) == 8
    assert parserfunc('2 + 4'.split()) == 6
    assert parserfunc('2 + 4 + 15'.split()) == 21
    assert parserfunc('2 * 4 + 15'.split()) == 23
    assert parserfunc('2 + 4 * 15'.split()) == 62
    assert parserfunc('( 2 + 4 ) * 15'.split()) == 90
    assert parserfunc('2 + ( 4 * 15 )'.split()) == 62
    assert parserfunc('2 * ( 4 + 15 )'.split()) == 38
    assert parserfunc('2 * ( 4 + 15 ) * 3 '.split()) == 114
    assert parserfunc('( 4 * ( 3 + 7 ) ) * 3 '.split()) == 120


test_parser(parse_expr)
