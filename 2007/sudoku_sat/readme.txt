##
#
# A Sudoku solver written in Ruby.
# The Sudoku is converted into a SAT (satisfiability) problem in CNF
# notation, which is then solved using a simple DPLL SAT solver.
# 
# by Eli Bendersky. License: LGPL
#
##
#

In this package you will find a complete Sudoku solver implemented in Ruby.
This solver is different from the rest, since it translates Sudoku to 
a boolean satisfiability (http://en.wikipedia.org/wiki/Boolean_satisfiability_problem) 
problem (SAT) and solves this new problem. There is no practical value
whatsoever in this code. It did, however, have an educational value for me
when I researched the problem and implemented it. I hope someone with similar
curiosities will find it interesting. 

Read the rest of this document to learn how this code came to exist, and how to use it.

Some history:

In 2004 when I was still working in IBM I had a task of doing some
coding for a SAT solver. 

To learn more about SAT solvers, I read some papers and tutorials about
DPLL - an algorithm to solve SAT which is used as the basis for many
modern solvers. To consolidate my understanding of the algorithm, I wrote
a short document (dpll.pdf) and coded the algorithm in Perl. The implementation
is very simplistic because no heuristic is used. Real SAT solvers are all about
complex, sophisticated heuristics.

When Sudoku became popular, I solved a few and quickly reached a conclusion
that I don't find these puzzles interesting. The solution process just felt
too mechanic, and reminded me of manually solving a boolean formula for
satisfiability (SAT) - the same approach of applying clearly defined inference
rules over and over again until a solution is reached. So I searched the 
net a little (the Google search "sudoku sat" should get you started) and 
came upon a few interesting websites and articles that discuss representing
Sudoku as a SAT problem. Since I already had a (very) modest SAT solver of
my own, I decided to implement the other half of the problem and make it 
all work together to actually solve Sudokus.

At the same time I started being interested in Ruby, so I wanted to combine
the interests and code the solver in that language. I've rewritten my Perl
SAT solver in Ruby and added a Sudoku solver, which is capable of solving
even the hardest Sudokus without any guessing, as far as I know of.

The files in this package are:

SAT solving:

  - dpll.pdf: The short summary of SAT solving using the DPLL method
  - sat_solver.rb: A Ruby SAT solver based on dpll.pdf
  - test_sat_solver.rb: Unit testing for sat_solver.rb
  
Sudoku solving:

  - sudoku_solver.rb: A Ruby Sudoku solver. Takes a Sudoku problem in textual
    form and turns it into a SAT problem (the code is well commented and
    should be understandable). Then, uses the SAT solver in sat_solver.rb to
    actually solve the problem.
  - test_sudoku_solver.rb: Unit testing for sudoku_solver.rb
  
General:

  - readme.txt: This document.
  - eliben_utils.rb: Some useful Ruby code for the solvers to use.

I will be pleased to receive questions, bug reports or just feedback 
to eliben@gmail.com

Eli Bendersky
