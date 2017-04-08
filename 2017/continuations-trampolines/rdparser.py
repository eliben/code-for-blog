# Recursive-descent parser code sample, demonstrating a straightforward approach
# followed by CPS conversion.
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
        # This approach can be easily extended to support more operators.
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
    else:
        raise RuntimeError('unexpected token', tok)


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
    print('test OK')


# The following functions implement the same RD parser, but only have tail
# calls, due to CPS transform. This isn't the most convenient way to write such
# code - it serves as demonstartion for the blog post.
def parse_expr_cps(tokens, cont):
    def lval_cont(lval):
        if len(tokens) == 0:
            return cont(lval)
        elif tokens[0] != '+':
            return cont(lval)
        else:
            op = lambda a, b: a + b
            tokens.pop(0)
            return parse_expr_cps(tokens, lambda rval: cont(op(lval, rval)))
    return parse_term_cps(tokens, lval_cont)


def parse_term_cps(tokens, cont):
    def lval_cont(lval):
        if len(tokens) == 0:
            return cont(lval)
        elif tokens[0] != '*':
            return cont(lval)
        else:
            op = lambda a, b: a * b
            tokens.pop(0)
            return parse_term_cps(tokens, lambda rval: cont(op(lval, rval)))
    return parse_factor_cps(tokens, lval_cont)


def parse_factor_cps(tokens, cont):
    tok = tokens.pop(0)
    try:
        toknum = int(tok)
        return cont(toknum)
    except ValueError:
        pass

    def inparens_parse_cont(value):
        if tokens[0] != ')':
            raise RuntimeError('expected matching ")", got', tokens[0])
        tokens.pop(0)
        return cont(value)

    if tok == '(':
        return parse_expr_cps(tokens, inparens_parse_cont)
    else:
        raise RuntimeError('unexpected token', tok)


if __name__ == '__main__':
    e = '2 + 3 * ( 5 + 4 * 2 )'
    etoks = e.split()
    print('parsing:', etoks)
    print(parse_expr(etoks))
    print(parse_expr_cps(e.split(), lambda v: v))

    test_parser(parse_expr)
    test_parser(lambda toks: parse_expr_cps(toks, lambda v: v))
