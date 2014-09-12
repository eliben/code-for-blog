""" Based on the AIMA Python code.
"""
from collections import deque


class CSP(object):
    """ A finite-domain Constraint Satisfaction Problem.
        Specified by the following:
        
        vars:   
            A list of variables (ints or strings)
        domains:
            A dict holding for each var a list of its possible
            values.
        neighbors: 
            A dict holding for each var a list of the other 
            variables that participate in constraints with this
            var.
        binary_constraint:
            A function func(A, a, B, b) that returns True iff 
            neighbors A, B satisfy the constraint when they have
            values A=a, B=b.
        global_constraint:
            A function func(new_asgn, cur_asgn) that returns 
            True iff the var=val in new_asgn (a dict of var=val)
            conflict with the current assignment.
        
        The solution of a CSP is an 'assignment' - a dictionary
        mapping a variable to a value.
        
        Usage:
        
        Create a new CSP and then call a 'solve' method.
        
        Public interface:
        
        solve_search(init_assign=False, mcv=False, lcv=False):
            Solve the CSP using backtracking search with forward
            checking constraint propagation. 'init_assign' is
            an initial assignment from which the search starts 
            running (useful for problems like Sudoku).
            
            'mcv' turns the most constrained variable heuristic on
            'lcv' turns the least constraining value heuristic on
            
            Returns an assignment if one can be found, None 
            otherwise.
        
        check_consistency(assignment):
            Check that the given assignment is consistend (i.e.
            has no internal conflicts) in this CSP.
        
        to_str(assignment):
            Sub-classes may reimplement this to convert a solved
            CSP into a string.
        
        Attributes:
        
        nassigns:
            Statistical measure of the amount of assignments made
            during the search.
        
        loglevel:
            Set to enable logging printouts.
    """
    
    def __init__(self, vars, domains, neighbors, 
            binary_constraint=None, global_constraint=None):
        """ Construct a new CSP. If vars is empty, it becomes
            domains.keys()
        """
        self.vars = vars or domains.keys()
        self.domains = domains
        self.neighbors = neighbors
        
        self.binary_constraint = binary_constraint or (lambda *args: True)
        self.global_constraint = global_constraint or (lambda *args: True)
        
        self.clear()
        
    def clear(self):
        """ Clears the internal state of the CSP, allowing to run
            a solve method again.
        """
        # For forward checking
        self.curr_domains, self.pruned = {}, {}

        # For statistics 
        self.nassigns = 0
        
        # Minimal level of log messages to be printed
        self.loglevel = 999
        
    def to_str(self, assignment):
        raise NotImplementedError()
    
    def check_consistency(self, assignment):
        """ Checks that the assignment is consistent for this CSP.
            Returns True if it is, False if there are conflicts.
        """
        for var, val in assignment.iteritems():
            if self._has_conflicts(var, val, assignment):
                return False
        return True
    
    def solve_search(   self, init_assign=None, 
                        mcv=False, lcv=False):
        self.mcv = mcv
        self.lcv = lcv
        
        self.curr_domains, self.pruned = {}, {}
        for A in self.vars:
            self.curr_domains[A] = self.domains[A][:]
            self.pruned[A] = []
        
        def aux_backtrack(assignment):
            # Success if all the vars were assigned
            if len(assignment) == len(self.vars):
                return assignment
            
            # Pick a variable to assign next
            var = self._select_unassigned_var(assignment)
            
            # Try to assign the available domain values to this
            # variable. If an assignment doesn't immediately 
            # conflict, explore this path recursively.
            # If an assignment eventually fails, remove it and 
            # backtrack.
            #
            for val in self._order_domain_values(var, assignment):
                if not self._has_conflicts(var, val, assignment):
                    self._assign(var, val, assignment)
                    result = aux_backtrack(assignment)
                    if result is not None:
                        return result
                
                # This assignment didn't succeed, so backtrack.
                # Note: calling _unassign is harmless if this var
                # wasn't actually assigned by _assign
                #
                self._unassign(var, assignment)
    
            return None
        
        # Make initial assignment
        #
        assignment = {}
        if init_assign:
            self._log(20, '======== Initial assignment ========')
            for A in init_assign:
                self._assign(A, init_assign[A], assignment)
            self._log(20, '======== END Initial assignment ========')
            
        return aux_backtrack(assignment)
    
    def _log(self, level, msg):
        if level >= self.loglevel:
            print msg
    
    def _select_unassigned_var(self, assignment):
        """ Select the var to try to assign next.
        """
        if self.mcv:
            # Most constrained variable heuristic.
            # Pick the unassigned variable that has fewest legal
            # values remaining.
            #
            unassigned = [v for v in self.vars if v not in assignment]            
            return min(unassigned,
                key=lambda var: self._num_legal_values(var))
        else:
            # No heuristic: just select the first unassigned var
            #        
            for v in self.vars:
                if v not in assignment:
                    return v
    
    def _order_domain_values(self, var, assignment):
        """ Returns an iterator over the domain values to be tried
            for 'var' for the next assignment.
        """
        domain = self.curr_domains[var]
        if self.lcv:
            # Least constraining value heuristic.
            # Consider values with fewer conflicts first.
            # 
            key = lambda val: self._nconflicts(var, val, assignment)
            domain.sort(key=key)
        
        return domain

    def _nconflicts(self, var, val, assignment):
        """ The number of conflicts var=val has with the other
            variables.
            
            Note: subclasses may implement this more efficiently
            using domain knowledge.
        """
        return (
            0 + (not self.global_constraint({var: val}, assignment)) +
            sum(self._aux_neighbor_conflict(var, val, v, assignment) 
                    for v in self.neighbors[var]))

    def _assign(self, var, val, assignment):
        """ Add var=val to assignment. If var already has a value
            in the assignment, it's replaced.
        """
        assignment[var] = val
        
        self.nassigns += 1
        self._log(20, 'Assigning %s = %s' % (var, val))
    
        # Propagate constraints
        self._forward_check(var, val, assignment)
        
    def _unassign(self, var, assignment):
        """ Unassign var from the assignment. 
        """
        if var in assignment:
            self._log(20, 'Backtracking %s = %s' % (var, assignment[var]))
            
            # Restore the domains pruned from the previous value
            # tried for var.
            #
            for (B, b) in self.pruned[var]:
                self.curr_domains[B].append(b)
            self.pruned[var] = []

            del assignment[var]

    def _aux_neighbor_conflict(self, var, val, var2, assignment):
        """ Does var=val have conflicts with var2 in the 
            assignment?
        """
        val2 = assignment.get(var2, None)
        return not (    
            val2 is None or 
            self.binary_constraint(var, val, var2, val2))

    def _has_conflicts(self, var, val, assignment):
        """ Does var=val have conflicts with the assignment?
        """
        return (
            not self.global_constraint({var: val}, assignment) or 
            any(self._aux_neighbor_conflict(var, val, v, assignment) 
                    for v in self.neighbors[var]))

    def _num_legal_values(self, var):
        """ The amount of legal values possible for 'var'.
        """
        return len(self.curr_domains[var])

    def _forward_check(self, var, val, assignment):
        """ Do forward checking (constraint propagation) for 
            var=val with the given assignment.
        """
        # Remove all neighbor values that conflict with 
        # var=val from their curr_domains
        #
        for B in self.neighbors[var]:
            if B in assignment:
                continue
            for b in self.curr_domains[B]:
                if not (self.binary_constraint(var, val, B, b) and 
                        self.global_constraint({var: val, B: b}, assignment)):
                    self.curr_domains[B].remove(b)
                    self.pruned[var].append((B, b))
        
        self._log(10, 'FC: %s=%s, %s' % (var, val, assignment))
        self._log(10, '  : %s' % self.curr_domains)

