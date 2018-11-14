# Implementation of type inference.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import itertools

import ast


# Subclasses of Type represent types for micro-ML.
class Type:
    pass


class IntType(Type):
    def __str__(self):
        return 'Int'

    __repr__ = __str__

    def __eq__(self, other):
        return type(self) == type(other)


class BoolType(Type):
    def __str__(self):
        return 'Bool'

    __repr__ = __str__

    def __eq__(self, other):
        return type(self) == type(other)


class FuncType(Type):
    """Function (n-ary) type.

    Encapsulates a sequence of argument types and a single return type.
    """
    def __init__(self, argtypes, rettype):
        assert len(argtypes) > 0
        self.argtypes = argtypes
        self.rettype = rettype

    def __str__(self):
        if len(self.argtypes) == 1:
            return '({} -> {})'.format(self.argtypes[0], self.rettype)
        else:
            return '(({}) -> {})'.format(', '.join(map(str, self.argtypes)),
                                         self.rettype)

    __repr__ = __str__

    def __eq__(self, other):
        return (type(self) == type(other) and
                self.rettype == other.rettype and
                all(self.argtypes[i] == other.argtypes[i]
                    for i in range(len(self.argtypes))))


class TypeVar(Type):
    """A type variable."""
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    __repr__ = __str__

    def __eq__(self, other):
        return type(self) == type(other) and self.name == other.name


class TypingError(Exception):
    pass


# A symbol table is used to map symbols to types throughout the inference
# process. Example:
#
#  > eight = 8
#  > nine = 9
#  > foo a = if a == 0 then eight else nine
#
# When inferring the type for 'foo', we already have 'eight' and 'nine' assigned
# to IntType in the symbol table. Also, inside the definition of 'foo' we have
# 'a' assigned a TypeVar type (since the type of 'a' is initially unknown).

# Stages:
#
# 1. Visit the AST and assign types to all nodes: known types to constant nodes
#    and fresh typevars to all other nodes. The types are placed in the _type
#    attribute of each node.
# 2. Visit the AST again, this time applying type inference rules to generate
#    equations between the types. The result is a list of equations, all of
#    which have to be satisfied.
# 3. Find the most general unifier (solution) for these equations by using
#    the classical unification algorithm.

# Global counter to produce unique type names.
_typecounter = itertools.count(start=0)

def _get_fresh_typename():
    """Creates a fresh typename that will be unique throughout the program."""
    return 't{}'.format(next(_typecounter))


# This function is useful for determinism in tests.
def reset_type_counter():
    global _typecounter
    _typecounter = itertools.count(start=0)


def assign_typenames(node, symtab={}):
    """Assign typenames to the given AST subtree and all its children.

    Symtab is the initial symbol table we can query for identifiers found
    throughout the subtree. All identifiers in the subtree must be bound either
    in symtab or in lambdas contained in the subtree.

    This function doesn't return anything, but it updates the _type property
    on the AST nodes it visits.
    """
    if isinstance(node, ast.Identifier):
        # Identifier nodes are treated specially, as they have to refer to
        # previously defined identifiers in the symbol table.
        if node.name in symtab:
            node._type = symtab[node.name]
        else:
            raise TypingError('unbound name "{}"'.format(node.name))
    elif isinstance(node, ast.LambdaExpr):
        node._type = TypeVar(_get_fresh_typename())
        local_symtab = dict()
        for argname in node.argnames:
            typename = _get_fresh_typename()
            local_symtab[argname] = TypeVar(typename)
        node._arg_types = local_symtab
        assign_typenames(node.expr, {**symtab, **local_symtab})
    elif isinstance(node, ast.OpExpr):
        node._type = TypeVar(_get_fresh_typename())
        node.visit_children(lambda c: assign_typenames(c, symtab))
    elif isinstance(node, ast.IfExpr):
        node._type = TypeVar(_get_fresh_typename())
        node.visit_children(lambda c: assign_typenames(c, symtab))
    elif isinstance(node, ast.AppExpr):
        node._type = TypeVar(_get_fresh_typename())
        node.visit_children(lambda c: assign_typenames(c, symtab))
    elif isinstance(node, ast.IntConstant):
        node._type = IntType()
    elif isinstance(node, ast.BoolConstant):
        node._type = BoolType()
    else:
        raise TypingError('unknown node {}', type(node))


def show_type_assignment(node):
    """Show a type assignment for the given subtree, as a table.

    Returns a string that shows the assigmnent.
    """
    lines = []

    def show_rec(node):
        lines.append('{:60} {}'.format(str(node), node._type))
        node.visit_children(show_rec)

    show_rec(node)
    return '\n'.join(lines)


class TypeEquation:
    """A type equation between two types: left and right.

    orig_node is the original AST node from which this equation was derived, for
    debugging.
    """
    def __init__(self, left, right, orig_node):
        self.left = left
        self.right = right
        self.orig_node = orig_node

    def __str__(self):
        return '{} :: {} [from {}]'.format(self.left,
                                           self.right, self.orig_node)

    __repr__ = __str__


def generate_equations(node, type_equations):
    """Generate type equations from node and place them in type_equations.

    Prior to calling this functions, node and its children already have to
    be annotated with _type, by a prior call to assign_typenames.
    """
    if isinstance(node, ast.IntConstant):
        type_equations.append(TypeEquation(node._type, IntType(), node))
    elif isinstance(node, ast.BoolConstant):
        type_equations.append(TypeEquation(node._type, BoolType(), node))
    elif isinstance(node, ast.Identifier):
        # Identifier references add no equations.
        pass
    elif isinstance(node, ast.OpExpr):
        node.visit_children(lambda c: generate_equations(c, type_equations))
        # All op arguments are integers.
        type_equations.append(TypeEquation(node.left._type, IntType(), node))
        type_equations.append(TypeEquation(node.right._type, IntType(), node))
        # Some ops return boolean, and some return integer.
        if node.op in {'!=', '==', '>=', '<=', '>', '<'}:
            type_equations.append(TypeEquation(node._type, BoolType(), node))
        else:
            type_equations.append(TypeEquation(node._type, IntType(), node))
    elif isinstance(node, ast.AppExpr):
        node.visit_children(lambda c: generate_equations(c, type_equations))
        argtypes = [arg._type for arg in node.args]
        # An application forces its function's type.
        type_equations.append(TypeEquation(node.func._type,
                                           FuncType(argtypes, node._type),
                                           node))
    elif isinstance(node, ast.IfExpr):
        node.visit_children(lambda c: generate_equations(c, type_equations))
        type_equations.append(TypeEquation(node.ifexpr._type, BoolType(), node))
        type_equations.append(TypeEquation(node._type, node.thenexpr._type, node))
        type_equations.append(TypeEquation(node._type, node.elseexpr._type, node))
    elif isinstance(node, ast.LambdaExpr):
        node.visit_children(lambda c: generate_equations(c, type_equations))
        argtypes = [node._arg_types[name] for name in node.argnames]
        type_equations.append(
            TypeEquation(node._type,
                         FuncType(argtypes, node.expr._type), node))
    else:
        raise TypingError('unknown node {}', type(node))



def unify(typ_x, typ_y, subst):
    """Unify two types typ_x and typ_y, with initial subst.

    Returns a subst (map of name->Type) that unifies typ_x and typ_y, or None if
    they can't be unified. Pass subst={} if no subst are initially
    known. Note that {} means valid (but empty) subst.
    """
    if subst is None:
        return None
    elif typ_x == typ_y:
        return subst
    elif isinstance(typ_x, TypeVar):
        return unify_variable(typ_x, typ_y, subst)
    elif isinstance(typ_y, TypeVar):
        return unify_variable(typ_y, typ_x, subst)
    elif isinstance(typ_x, FuncType) and isinstance(typ_y, FuncType):
        if len(typ_x.argtypes) != len(typ_y.argtypes):
            return None
        else:
            subst = unify(typ_x.rettype, typ_y.rettype, subst)
            for i in range(len(typ_x.argtypes)):
                subst = unify(typ_x.argtypes[i], typ_y.argtypes[i], subst)
            return subst
    else:
        return None


def occurs_check(v, typ, subst):
    """Does the variable v occur anywhere inside typ?

    Variables in typ are looked up in subst and the check is applied
    recursively.
    """
    assert isinstance(v, TypeVar)
    if v == typ:
        return True
    elif isinstance(typ, TypeVar) and typ.name in subst:
        return occurs_check(v, subst[typ.name], subst)
    elif isinstance(typ, FuncType):
        return (occurs_check(v, typ.rettype, subst) or
                any(occurs_check(v, arg, subst) for arg in typ.argtypes))
    else:
        return False


def unify_variable(v, typ, subst):
    """Unifies variable v with type typ, using subst.

    Returns updated subst or None on failure.
    """
    assert isinstance(v, TypeVar)
    if v.name in subst:
        return unify(subst[v.name], typ, subst)
    elif isinstance(typ, TypeVar) and typ.name in subst:
        return unify(v, subst[typ.name], subst)
    elif occurs_check(v, typ, subst):
        return None
    else:
        # v is not yet in subst and can't simplify x. Extend subst.
        return {**subst, v.name: typ}


def unify_all_equations(eqs):
    """Unifies all type equations in the sequence eqs.

    Returns a substitution (most general unifier).
    """
    subst = {}
    for eq in eqs:
        subst = unify(eq.left, eq.right, subst)
        if subst is None:
            break
    return subst


def apply_unifier(typ, subst):
    """Applies the unifier subst to typ.

    Returns a type where all occurrences of variables bound in subst
    were replaced (recursively); on failure returns None.
    """
    if subst is None:
        return None
    elif len(subst) == 0:
        return typ
    elif isinstance(typ, (BoolType, IntType)):
        return typ
    elif isinstance(typ, TypeVar):
        if typ.name in subst:
            return apply_unifier(subst[typ.name], subst)
        else:
            return typ
    elif isinstance(typ, FuncType):
        newargtypes = [apply_unifier(arg, subst) for arg in typ.argtypes]
        return FuncType(newargtypes,
                        apply_unifier(typ.rettype, subst))
    else:
        return None


def get_expression_type(expr, subst, rename_types=False):
    """Finds the type of the expression given a substitution.

    If rename_types is True, renames all the type vars to be sequential
    characters starting from 'a', so that 't5 -> t3' will be renamed to
    'a -> b'. These names are less cluttery and also facilitate testing.

    Note: expr should already be annotated with assign_typenames.
    """
    typ = apply_unifier(expr._type, subst)
    if rename_types:
        namecounter = itertools.count(start=0)
        namemap = {}
        def rename_type(typ):
            nonlocal namecounter
            if isinstance(typ, TypeVar):
                if typ.name in namemap:
                    typ.name = namemap[typ.name]
                else:
                    name = chr(ord('a') + next(namecounter))
                    namemap[typ.name] = name
                    namemap[name] = name
                    typ.name = namemap[typ.name]
            elif isinstance(typ, FuncType):
                rename_type(typ.rettype)
                for argtyp in typ.argtypes:
                    rename_type(argtyp)
        rename_type(typ)
    return typ
