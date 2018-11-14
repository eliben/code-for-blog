# Run this file as: python3 main.py
#
# It will print out its progress of type inference for a code snippet.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import ast
import parser
import typing


if __name__ == '__main__':
    code = 'foo f g x = if f(x == 1) then g(x) else 20'
    print('Code', '----', code, '', sep='\n')

    # Parse the microml code snippet into an AST.
    p = parser.Parser()
    e = p.parse_decl(code)
    print('Parsed AST', '----', e, '', sep='\n')

    # Stage 1: Assign symbolic typenames
    typing.assign_typenames(e.expr)
    print('Typename assignment', '----',
          typing.show_type_assignment(e.expr), '', sep='\n')

    # Stage 2: Generate a list of type equations
    equations = []
    typing.generate_equations(e.expr, equations)
    print('Equations', '----', sep='\n')
    for eq in equations:
        print('{:15} {:20} | {}'.format(str(eq.left), str(eq.right), eq.orig_node))

    # Stage 3: Solve equations using unification
    unifier = typing.unify_all_equations(equations)
    print('', 'Inferred type', '----',
          typing.get_expression_type(e.expr, unifier, rename_types=True),
          sep='\n')
