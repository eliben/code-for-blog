import sys

from csplib import CSP
from csp_sample_problems import *


def stringrep(asgn):
    # Groups in clockwise order starting with one particular...
    #
    groups = [[1, 3, 5], [2, 5, 8], [9, 8, 7], [10, 7, 4], [6, 4, 3]]

    start_group = None
    start_outer = 999
    for i, gr in enumerate(groups):
        if asgn[gr[0]] < start_outer:
            start_outer = asgn[gr[0]]
            start_group = i
    
    s = ''
    for i in xrange(start_group, start_group + len(groups)):
        ngroup = i % len(groups)
        #~ print ngroup
        
        for idx in groups[ngroup]:
            s += str(asgn[idx])
    
    return len(s), s
        
    
    
    


mg = make_magic_5gon_CSP()
sol = mg.solve_search({1: 10, 2: 9}, mcv=True)
print mg.to_str(sol)

print stringrep(sol)




