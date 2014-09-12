""" A collection of "worlds" suitable by solution by a CSP.
    Each world has a make_XXX_SCP function that creates a new
    CSP object, and some auxiliary utilities.
"""
import re, math
from collections import defaultdict
from types import StringTypes
from csplib import CSP


#----------------------------------------------------------------
#
# Map coloring
#

def unequal_vals_constraint(A, a, B, b):
    """ A simple constraint: two neighbors must always have 
        different values.
    """
    return a != b
    

def make_map_coloring_CSP(colors, neighbors):
    if isinstance(neighbors, StringTypes):
        neighbors = parse_neighbors_graph(neighbors)
        
    return CSP(
            vars=neighbors.keys(), 
            domains=defaultdict(lambda: colors),
            neighbors=neighbors,
            binary_constraint=unequal_vals_constraint)
        

def parse_neighbors_graph(neighbors, vars=[]):
    """ A utility for converting a string of the form 
        'X: Y Z; Y: Z' into a dict mapping variables to their
        neighbors. The syntax is a vertex name followed by a 
        ':' followed by zero or more vertex names, followed by
        ';', repeated for each vertes. Neighborhood is 
        commutative.
        
        'vars' may contain vertices that have no neighbors.
    """
    graph = defaultdict(list)
    for var in vars:
        graph[var] = []
    specs = [spec.split(':') for spec in neighbors.split(';')]
    for (v, v_neighbors) in specs:
        v = v.strip()
        graph.setdefault(v, [])
        for u in v_neighbors.split():
            graph[v].append(u)
            graph[u].append(v)
    return graph


def make_australia_CSP():
    #
    # WA---NT---Q
    #   \   |  / \
    #    \  | /   \
    #     \ |/     \
    #      SA------NSW
    #       \     /
    #        \   /
    #         \ /
    #          V
    #
    #
    #          T
    #
    
    return make_map_coloring_CSP(
        list('RGB'),
        'SA: WA NT Q NSW V; NT: WA Q; NSW: Q V; T: ')

    
def make_USA_CSP():
    return make_map_coloring_CSP(list('RGBY'),
        """WA: OR ID; OR: ID NV CA; CA: NV AZ; NV: ID UT AZ; ID: MT WY UT;
        UT: WY CO AZ; MT: ND SD WY; WY: SD NE CO; CO: NE KA OK NM; NM: OK TX;
        ND: MN SD; SD: MN IA NE; NE: IA MO KA; KA: MO OK; OK: MO AR TX;
        TX: AR LA; MN: WI IA; IA: WI IL MO; MO: IL KY TN AR; AR: MS TN LA;
        LA: MS; WI: MI IL; IL: IN; IN: KY; MS: TN AL; AL: TN GA FL; MI: OH;
        OH: PA WV KY; KY: WV VA TN; TN: VA NC GA; GA: NC SC FL;
        PA: NY NJ DE MD WV; WV: MD VA; VA: MD DC NC; NC: SC; NY: VT MA CT NJ;
        NJ: DE; DE: MD; MD: DC; VT: NH MA; MA: NH RI CT; CT: RI; ME: NH;
        HI: ; AK: """)


#----------------------------------------------------------------
#
# N-Queens
#
# (var, val) is (column, row)
# The domain is 0..N-1
#

def queens_constraint(A, a, B, b):
    """ Constraint is satisfied if it's the same column (queens
        are assigned by columns), or if the queens are not in the
        same row or diagonal
    """
    if A == B:
        return True
    return a != b and A + a != B + b and A - a != B - b


class NQueensCSP(CSP):
    def to_str(self, assignment):
        s = ''
        for row in self.domains:
            for col in self.vars:
                if assignment[col] == row:
                    s += '*'
                else:
                    s += 'o'
            s += '\n'
        return s


def make_NQueens_CSP(n):
    """ Creates a N-Queens CSP problem for a given N.
        
        Note that this isn't a particularly efficient 
        representation.
    """
    # columns
    vars = list(range(n))
    
    # rows
    domains = list(range(n))
    
    neighbors = {}
    for v in vars:
        neighbors[v] = vars[:]
        neighbors[v].remove(v)
    
    return NQueensCSP(
        vars=vars,
        domains=defaultdict(lambda: domains),
        neighbors=neighbors,
        binary_constraint=queens_constraint)
        
    
#----------------------------------------------------------------
#
# Sudoku
#
# Vars are (row, col) pairs.
# Values are 1..9
# 


class SudokuCSP(CSP):
    def to_str(self, assignment):
        s = ''
        for row in range(9):
            if row % 3 == 0:
                s += '+-------+-------+-------+\n'
            s += '| '
            for col in range(9):
                
                if (row, col) in assignment:
                    s += str(assignment[(row, col)])
                else:
                    s += '_'
                
                if col % 3 == 2:
                    s += ' | '
                else:
                    s += ' '
            s += '\n'
        s += '+-------+-------+-------+\n'
        return s


def cross(A, B):
    return [(a, b) for a in A for b in B]


def parse_sudoku_assignment(grid):
    """ Given a string of 81 digits, return the assignment it 
        represents. 0 means unassigned.
        Whitespace is ignored.
    """
    digits = re.sub('\s', '', grid)
    assert len(digits) == 81
    
    digit = iter(digits)
    
    asg = {}
    for row in range(9):
        for col in range(9):
            d = int(digit.next())
            if d > 0:
                asg[(row, col)] = d
    
    return asg


def make_sudoku_CSP():
    """ A regular 9x9 Sudoku puzzle. Note that it's an 'empty'
        Sudoku board. Solving partially filled boards is done
        by passing an initial assignment (obtained with 
        parse_sudoku_assignment) to the solve_search method of 
        the CSP.
    """
    # All (row, col) cells
    rows = range(9)
    cols = range(9)
    vars = cross(rows, cols)
    
    # Available values
    domains = defaultdict(lambda: range(1, 10))

    triples = [[0, 1, 2], [3, 4, 5], [6, 7, 8]]
    unitlist = ([cross(rows, [c]) for c in cols] +
                [cross([r], cols) for r in rows] + 
                [cross(rs, cs) for rs in triples for cs in triples])
    
    # Neighbors holds sets, but that's fine for CSP - it just 
    # wants 'em to be iterable
    #
    neighbors = defaultdict(lambda: set([]))
    for unit in unitlist:
        for cell in unit:
            neighbors[cell].update(unit)
            neighbors[cell].remove(cell)
    
    return SudokuCSP(
        vars=vars,
        domains=domains,
        neighbors=neighbors,
        binary_constraint=unequal_vals_constraint)
    

#----------------------------------------------------------------
#
# Magic squares
#

class MagicSquareCSP(CSP):
    def to_str(self, assignment):
        s = ''
        ns = range(int(math.sqrt(len(self.vars))))
        for row in ns:
            for col in ns:
                if (row, col) in assignment:
                    s += str(assignment[(row, col)])
                else:
                    s += '_'
                
                s += ' '
            s += '\n'
        return s


def make_magic_square_CSP(n):
    """ A NxN additive magic square
    
        A sample solution for 3x3:
        
        2 7 6
        9 5 1
        4 3 8
        
        (row, column and diagonal sum = 15)
    """
    rows = range(n)
    cols = range(n)
    vars = cross(rows, cols)
    
    domains = defaultdict(lambda: range(1, n*n + 1))
    magic_sum = n * (n*n + 1) / 2
    
    # All cells are different --> neighbors of one another.
    #
    neighbors = {}
    for v in vars:
        neighbors[v] = vars[:]
        neighbors[v].remove(v)
    
    def check_sum(values):
        s = sum(values)
        if s > magic_sum:
            return False
        return not (len(values) == n and s != magic_sum)

    def sum_constraint(new_asgn, cur_asgn):        
        square = {}
        square.update(new_asgn)
        square.update(cur_asgn)
        
        # Only new assignments can cause conflicts...
        #
        for (vrow, vcol) in new_asgn.iterkeys():
            #~ if check_sum([square.get((vrow, col), 0) for col in cols]) == False:
            if check_sum([square[(vrow, col)] for col in cols if (vrow, col) in square]) == False:
                return False
        
            #~ if check_sum([square.get((row, vcol), 0) for row in rows]) == False:
            if check_sum([square[(row, vcol)] for row in rows if (row, vcol) in square]) == False:
                return False
        
            # \ diagonal
            if (    vrow == vcol and
                    check_sum([square[(row, row)] for row in rows if (row, row) in square]) == False):
                return False
        
            # / diagonal
            if (    vrow == n - 1 - vcol and
                    check_sum([square[(n - 1 - row, row)] 
                        for row in rows 
                            if (n - 1 - row, row) in square]) == False):
                return False
        
        return True
    
    return MagicSquareCSP(
        vars=vars,
        domains=domains,
        neighbors=neighbors,
        binary_constraint=unequal_vals_constraint,
        global_constraint=sum_constraint)

#----------------------------------------------------------------
#
# Magic gons (Project Euler problem 68)
#

class Magic3gonCSP(CSP):
    def to_str(self, assignment):
        asgn = defaultdict(lambda: '*')
        asgn.update(assignment)
        
        s = ''
        s += ' %s\n' % asgn[2]
        s += '\n'
        s += '  %s\n' % asgn[4]
        s += '\n'
        s += ' %s  %s  %s\n' % (asgn[3], asgn[5], asgn[6])
        s += '\n'
        s += '%s\n' % asgn[1]
        
        return s

def make_magic_3gon_CSP():
    vars = range(1, 7)
    domains = defaultdict(lambda: vars)
    
    # All cells are different --> neighbors of one another.
    #
    neighbors = {}
    for v in vars:
        neighbors[v] = vars[:]
        neighbors[v].remove(v)

    groups = [[1, 3, 4], [2, 4, 5], [6, 5, 3]]

    def sum_constraint(new_asgn, cur_asgn):
        asgn = defaultdict(lambda: 999)
        asgn.update(new_asgn)
        asgn.update(cur_asgn)
        
        last_total = None
        for group in groups:
            total = sum(asgn[i] for i in group)
            
            if total < 1000:
                if last_total is None:
                    last_total = total
                elif last_total != total:
                    return False
        
        return True
    
    return Magic3gonCSP(
        vars=vars,
        domains=domains,
        neighbors=neighbors,
        binary_constraint=unequal_vals_constraint,
        global_constraint=sum_constraint)
        

class Magic5gonCSP(CSP):
    def to_str(self, assignment):
        asgn = defaultdict(lambda: '*')
        asgn.update(assignment)
        
        s = ''
        s += '  %s    %s\n' % (asgn[2], asgn[9])
        s += '    %s\n' % asgn[5]
        s += '  %s   %s\n' % (asgn[3], asgn[8])
        s += '%s\n' % (asgn[1])
        s += '   %s %s %s\n' % (asgn[4], asgn[7], asgn[10])
        s += '\n'
        s += '    %s\n' % asgn[6]
        
        return s

def make_magic_5gon_CSP():
    vars = range(1, 11)
    domains = defaultdict(lambda: vars)
    
    # All cells are different --> neighbors of one another.
    #
    neighbors = {}
    for v in vars:
        neighbors[v] = vars[:]
        neighbors[v].remove(v)

    groups = [[1, 3, 5], [2, 5, 8], [9, 8, 7], [10, 7, 4], [6, 4, 3]]

    def sum_constraint(new_asgn, cur_asgn):
        asgn = defaultdict(lambda: 999)
        asgn.update(new_asgn)
        asgn.update(cur_asgn)
        
        last_total = None
        for group in groups:
            total = sum(asgn[i] for i in group)
            
            if total < 1000:
                if last_total is None:
                    last_total = total
                elif last_total != total:
                    return False
        
        return True
    
    return Magic5gonCSP(
        vars=vars,
        domains=domains,
        neighbors=neighbors,
        binary_constraint=unequal_vals_constraint,
        global_constraint=sum_constraint)

        










