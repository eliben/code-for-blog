lexer.py:
  A generic Lexer class useful for tokenizing strings.

rd_parser_bnf.py:
  A naive recursive descent parser implementation based
  on a BNF grammar. It suffers from an operator associativity
  problem.

rd_parser_ebnf.py:
  A more sophisticated RD parser based on an EBNF grammar
  and without the associativity problem. It also supports
  much more operations.

rd_parser_infix_expr.py
  A hybrid RD parser combined with an infix expression
  evaluator for the expressions.

test_rd_parser.py
  Unit-tests for the parser. Can be used to check the
  correctness of parsers supporting the grammar defined
  in rd_parser_ebnf.py
