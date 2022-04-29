# Epsilon production removal from grammars
#
# Eli Bendersky [https://eli.thegreenplace.net]
# This code is in the public domain.
import sys, os
from collections import defaultdict


class CFG(object):
    def __init__(self):
        self.prod = defaultdict(list)
        self.start = None

    def set_start_symbol(self, start):
        """ Set the start symbol of the grammar.
        """
        self.start = start

    def add_prod(self, lhs, rhs):
        """ Add production to the grammar. 'rhs' can
            be several productions separated by '|'.
            Each production is a sequence of symbols
            separated by whitespace.
            Empty strings are interpreted as an eps-production.

            Usage:
                grammar.add_prod('NT', 'VP PP')
                grammar.add_prod('Digit', '1|2|3|4')

                # Optional Digit: digit or eps
                grammar.add_prod('Digit_opt', Digit |')
        """
        # The internal data-structure representing productions.
        # maps a nonterminal name to a list of productions, each
        # a list of symbols. An empty list [] specifies an
        # eps-production.
        #
        prods = rhs.split('|')
        for prod in prods:
            self.prod[lhs].append(prod.split())

    def remove_eps_productions(self):
        """ Removes epsilon productions from the grammar.

            The algorithm:

            1. Pick a nonterminal p_eps with an epsilon production
            2. Remove that epsilon production
            3. For each production containing p_eps, replace it
               with several productions such that all the
               combinations of p_eps being there or not will be
               represented.
            4. If there are still epsilon productions in the
               grammar, go back to step 1

            The replication can be demonstrated with an example.
            Suppose that A contains an epsilon production, and
            we've found a production B:: [A, k, A]
            Then this production of B will be replaced with these:
            [A, k], [k], [k, A], [A, k, A]
        """
        while True:
            # Find an epsilon production
            #
            p_eps, index = self._find_eps_production()

            # No epsilon productions? Then we're done...
            #
            if p_eps is None:
                break

            # Remove the epsilon production
            #
            del self.prod[p_eps][index]

            # Now find all the productions that contain the
            # production that removed.
            # For each such production, replicate it with all
            # the combinations of the removed production.
            #
            for lhs in self.prod:
                prods = []

                for lhs_prod in self.prod[lhs]:
                    num_p_eps = lhs_prod.count(p_eps)
                    if num_p_eps == 0:
                        prods.append(lhs_prod)
                    else:
                        prods.extend(self._create_prod_combinations(
                            prod=lhs_prod,
                            nt=p_eps,
                            count=num_p_eps))

                # Remove duplicates
                #
                prods = sorted(prods)
                prods = [prods[i] for i in xrange(len(prods))
                                  if i == 0 or prods[i] != prods[i-1]]

                self.prod[lhs] = prods

    def _find_eps_production(self):
        """ Finds an epsilon production in the grammar. If such
            a production is found, returns the pair (lhs, index):
            the name of the non-terminal that has an epsilon
            production and its index in lhs's list of productions.
            If no epsilon productions were found, returns the
            pair (None, None).

            Note: eps productions in the start symbol will be
            ignored, because we don't want to remove them.
        """
        for lhs in self.prod:
            if not self.start is None and lhs == self.start:
                continue

            for i, p in enumerate(self.prod[lhs]):
                if len(p) == 0:
                    return lhs, i

        return None, None

    def _create_prod_combinations(self, prod, nt, count):
        """ prod:
                A production (list) that contains at least one
                instance of 'nt'
            nt:
                The non-terminal which should be replicated
            count:
                The amount of times 'nt' appears in 'lhs_prod'.
                Assumed to be >= 1

            Returns the generated list of productions.
        """
        # The combinations are a kind of a powerset. Membership
        # in a powerset can be checked by using the binary
        # representation of a number.
        # There are 2^count possibilities in total.
        #
        numset = 1 << count
        new_prods = []

        for i in xrange(numset):
            nth_nt = 0
            new_prod = []

            for s in prod:
                if s == nt:
                    if i & (1 << nth_nt):
                        new_prod.append(s)
                    nth_nt += 1
                else:
                    new_prod.append(s)

            new_prods.append(new_prod)

        return new_prods



#-----------------------------------------------------------------
if __name__ == "__main__":
    cfg = CFG()
    #~ cfg.add_prod('B', 'A z A | A p | c r')
    #~ cfg.add_prod('A', 'a | | c k')

    #~ cfg.set_start_symbol('S')
    #~ cfg.add_prod('S', 'A B | A')
    #~ cfg.add_prod('A', 'A a | | C c')
    #~ cfg.add_prod('B', 'C | b')
    #~ cfg.add_prod('C', 'C v | w |')

    cfg.add_prod('func_call', 'identifier ( arguments_opt )')
    cfg.add_prod('arguments_opt', 'arguments_list | ')
    cfg.add_prod('arguments_list', 'argument | argument , arguments_list')

    #~ cfg.add_prod('B', 'A z A')
    #~ cfg.add_prod('A', 'a | ')


    #~ cfg.add_prod('S', 'A B')
    #~ cfg.add_prod('A', 'A A B | | a')
    #~ cfg.add_prod('B', 'C D C | A | b |')
    #~ cfg.add_prod('C', 'c |')
    #~ cfg.add_prod('D', 'd')

    cfg.remove_eps_productions()
    for p in cfg.prod:
        print p, ':: ', [' '.join(pr) for pr in cfg.prod[p]]

    #~ print cfg._create_prod_combinations(['A', 'b', 'c', 'A'], 'A', 2)
