#-------------------------------------------------------------------------------
# Symbol tables in Python
#
# Eli Bendersky (eliben@gmail.com)
# This code is in the public domain
#-------------------------------------------------------------------------------
import symtable

def describe_symbol(sym):
    assert type(sym) == symtable.Symbol
    print("Symbol:", sym.get_name())

    for prop in [
            'referenced', 'imported', 'parameter',
            'global', 'declared_global', 'local',
            'free', 'assigned', 'namespace']:
        if getattr(sym, 'is_' + prop)():
            print('    is', prop)

def describe_symtable(st, recursive=True, indent=0):
    def print_d(s, *args):
        prefix = ' ' * indent
        print(prefix + s, *args)

    assert isinstance(st, symtable.SymbolTable)
    print_d('Symtable: type=%s, id=%s, name=%s' % (
                st.get_type(), st.get_id(), st.get_name()))
    print_d('  nested:', st.is_nested())
    print_d('  has children:', st.has_children())
    print_d('  identifiers:', list(st.get_identifiers()))
    print_d('  varnames:', st._table.varnames)

    if recursive:
        for child_st in st.get_children():
            describe_symtable(child_st, recursive, indent + 5)

#-----------------------------------------------------------------------------

code = '''
class A():
    cl_attr = 1
    def fn1(self):
        print('sdfsdfsdfsdfsdf')
        self.attr1 = 'hello'
    def fn2(self, arg):
        self.attr2 = arg
'''

#print("My compile...............")
#co = compile(code, '<string>', 'exec')
#for cn in co.co_consts:
    #try:
        #print("co_varnames=", cn.co_varnames)
    #except:
        #pass
table = symtable.symtable(code, '<string>', 'exec')
describe_symtable(table)

#import sys; sys.exit()

#func_symbol = table.lookup('foo')
#func_namespace = func_symbol.get_namespace()

#for s in func_namespace.get_identifiers():
    #describe_symbol(func_namespace.lookup(s))

#print('--------------')

#internal_func_symbol = func_namespace.lookup('inner')
#internal_func_namespace = internal_func_symbol.get_namespace()

#for s in internal_func_namespace.get_identifiers():
    #describe_symbol(internal_func_namespace.lookup(s))
