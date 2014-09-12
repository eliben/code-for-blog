##
#
# Unit testing for sudoku_solver.rb
# 
# by Eli Bendersky. License: LGPL
#
##
#
$stdout.sync = true
$stderr.sync = true

require 'benchmark'
require 'test/unit'
require 'sudoku_solver'


class TestSudokuSolver < Test::Unit::TestCase

    def setup
        @valid_canonical = '002005079105003000000000600010400900090000080004009010009000000000100306680300400'
        @valid_pretty = '
+-------+-------+-------+
| _ _ 2 | _ _ 5 | _ 7 9 |
| 1 _ 5 | _ _ 3 | _ _ _ |
| _ _ _ | _ _ _ | 6 _ _ |
+-------+-------+-------+
| _ 1 _ | 4 _ _ | 9 _ _ |
| _ 9 _ | _ _ _ | _ 8 _ |
| _ _ 4 | _ _ 9 | _ 1 _ |
+-------+-------+-------+
| _ _ 9 | _ _ _ | _ _ _ |
| _ _ _ | 1 _ _ | 3 _ 6 |
| 6 8 _ | 3 _ _ | 4 _ _ |
+-------+-------+-------+
'
        @valid_solution_pretty = '
+-------+-------+-------+
| 3 6 2 | 8 4 5 | 1 7 9 |
| 1 7 5 | 9 6 3 | 2 4 8 |
| 9 4 8 | 2 1 7 | 6 3 5 |
+-------+-------+-------+
| 7 1 3 | 4 5 8 | 9 6 2 |
| 2 9 6 | 7 3 1 | 5 8 4 |
| 8 5 4 | 6 2 9 | 7 1 3 |
+-------+-------+-------+
| 4 3 9 | 5 7 6 | 8 2 1 |
| 5 2 7 | 1 8 4 | 3 9 6 |
| 6 8 1 | 3 9 2 | 4 5 7 |
+-------+-------+-------+
'        
        @simple_pretty = '
+-------+-------+-------+
| _ _ 2 | _ 4 5 | _ 7 9 |
| 1 _ 5 | _ 6 3 | _ _ _ |
| _ _ _ | _ 1 _ | 6 _ _ |
+-------+-------+-------+
| _ 1 _ | 4 5 _ | 9 _ _ |
| _ 9 _ | _ 3 _ | _ 8 _ |
| _ _ 4 | _ 2 9 | _ 1 _ |
+-------+-------+-------+
| _ _ 9 | _ 7 _ | _ _ _ |
| _ _ _ | 1 8 _ | 3 9 6 |
| 6 8 _ | 3 9 _ | 4 5 _ |
+-------+-------+-------+
'
        @hard_canonical = '000000010400000000020000000000050407008000300001090000300400200050100000000806000'
        
        @hard_solution_pretty = '
+-------+-------+-------+
| 6 9 3 | 7 8 4 | 5 1 2 |
| 4 8 7 | 5 1 2 | 9 3 6 |
| 1 2 5 | 9 6 3 | 8 7 4 |
+-------+-------+-------+
| 9 3 2 | 6 5 1 | 4 8 7 |
| 5 6 8 | 2 4 7 | 3 9 1 |
| 7 4 1 | 3 9 8 | 6 2 5 |
+-------+-------+-------+
| 3 1 9 | 4 7 5 | 2 6 8 |
| 8 5 6 | 1 2 9 | 7 4 3 |
| 2 7 4 | 8 3 6 | 1 5 9 |
+-------+-------+-------+
'
    end
   
    def test_creation
        solver = nil
        
        # test some illegal initialization parameters
        assert_raise(RuntimeError) {solver = SudokuSolver.new("baba")}
        assert_raise(RuntimeError) {solver = SudokuSolver.new("baba", :kuritza)}
        
        # now test correct initialization
        assert_nothing_raised {solver = SudokuSolver.new}
        assert_nothing_raised {solver = SudokuSolver.new(@valid_canonical, :canonical)}
    end
    
    def test_load
        solver = SudokuSolver.new
        
        # illegal load calls
        assert_raise(RuntimeError) {solver.load_from_canonical('abc')}
        assert_raise(RuntimeError) {solver.load_from_canonical('5' * 82)}
        
        # valid canonical load
        assert_nothing_raised {solver.load_from_canonical(@valid_canonical)}
        assert_equal([1, 0, 5, 0, 0, 3, 0, 0, 0], solver.board[1])
        assert_equal([6, 8, 0, 3, 0, 0, 4, 0, 0], solver.board[8])
        assert_equal(2, solver.board[0][2])
        
        solver2 = SudokuSolver.new
        assert_nothing_raised {solver2.load_from_pretty(@valid_pretty)}
        
        # the valid_canonical and valid_pretty strings represent the same boards
        assert_equal(solver.board, solver2.board)

        assert_equal(@valid_canonical, solver.to_s_canonical)
        assert_equal(@valid_pretty, "\n" + solver.to_s_pretty)
        
        solver.load_from_canonical(@hard_canonical)
        assert_equal(@hard_canonical, solver.to_s_canonical)
    end
    
    def test_solutions
        solver = SudokuSolver.new
        solver.load_from_canonical(@valid_canonical)
        assert(solver.solve(:extended))
        assert_equal(@valid_solution_pretty, "\n" + solver.to_s_pretty)
        
        solver.load_from_canonical(@hard_canonical)
        assert(solver.solve(:extended))
        assert_equal(@hard_solution_pretty, "\n" + solver.to_s_pretty)
    end
        
    def test_test
        
        
        #
        #puts "\nElapsed >>> " + Benchmark.realtime {
        #    solver = SudokuSolver.new
        #    solver.load_from_canonical(@valid_canonical)
        #    res = solver.solve(:extended)
        #    pp res
        #}.to_s
        #
        #solver = SudokuSolver.new
        #solver.load_from_pretty(@simple_pretty)
        #cnf_formula = solver.make_cnf_formula(:extended)
        #
        #unit_clause = nil
        #
        #puts "\nElapsed >>> " + Benchmark.realtime {        
        #unit_clause = SatUtils.find_unit_clause(cnf_formula)
        #}.to_s
        #
        #puts unit_clause
    end
    
end
