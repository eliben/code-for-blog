##
#
# Unit testing for sat_solver.rb
# 
# by Eli Bendersky. License: LGPL
#
##
#
require 'sat_solver'
require 'test/unit'
require 'set'


class TestSatSolver < Test::Unit::TestCase
    
    def test_creation
        solver = nil
        assert_nothing_raised {solver = SatSolver.new([])}
        assert_equal(solver.formula, [])
        assert_raise(ArgumentError) {solver.read_formula(12)}
        assert_nothing_raised {solver.read_formula("-1 2 3\n6\t  4 -2 1  \n  \t5 -4  \t\n")}
        assert_equal([[-1, 2, 3], [6, 4, -2, 1], [5, -4]], solver.formula)
        assert_equal([1, 2, 3, 4, 5, 6], solver.formula_vars.sort)
    end
    
    def test_basic_ops
        frm = [[1, 2, 3], [-1, 2, -3], [-2, -3]]
        SatUtils.reduce_literal(frm, 1)
        assert_equal([[2, -3], [-2, -3]], frm)
        frm = [[1, 2, 3], [-1, 2, -3], [-2, -3]]
        SatUtils.reduce_literal(frm, -1)
        assert_equal([[2, 3], [-2, -3]], frm)
        assert_equal([-3], SatUtils.find_unit_clause([[1, 2], [-3], [2, 3], [-4]]))
        assert_equal([-4], SatUtils.find_unit_clause([[1, 2], [-3, 2], [2, 3], [-4]]))
        assert_nil(SatUtils.find_unit_clause([[1, 2], [-3, 2], [2, 3], [2, -4]]))
        assert(SatUtils.has_contradiction?([[1, 2], [], [-1]]))
        assert(!SatUtils.has_contradiction?([[1, 2], [1], [-1]]))

        formula = [[1, -2, 3], [-4], [-1, 4], [-1, -2, -3, 4]]
        assert_equal([-4], SatUtils.find_unit_clause(formula))
        SatUtils.reduce_literal(formula, -4)
        assert_equal([[1, -2, 3], [-1], [-1, -2, -3]], formula)
        assert_equal([-1], SatUtils.find_unit_clause(formula))
        SatUtils.reduce_literal(formula, -1)
        assert_equal([[-2, 3]], formula)
    end
    
    def test_unit_propagation
        assert_equal([[], [1, 2]], SatUtils.unit_propagation([[1], [2]]))
        frm, vars = SatUtils.unit_propagation([[5, 6, 7, 8], [-5, -6], [-2, -4], [4, 8]])
        assert_equal([], vars)
        assert_equal([[5, 6, 7, 8], [-5, -6], [-2, -4], [4, 8]], frm)
        assert_equal([[], [1, 2, 3, 4]], SatUtils.unit_propagation([[1], [-1, 2], [-1, -2, 3], [-1, -2, -3, 4]]))
        frm, vars = SatUtils.unit_propagation([[1, -2, 3], [-4], [-1, 4], [-1, -2, -3, 4]])
        assert_equal([-4, -1], vars)
        assert_equal([[-2, 3]], frm)
            
    end
        
    def test_inferences
        solver = SatSolver.new
        
        # failed_literal? also runs unit_propagation
        assert(solver.failed_literals?([[1, 2, 3], [-1]], [1]))
        assert(!solver.failed_literals?([[1, 2, 3], [-1]], [-1]))
        assert(!solver.failed_literals?([[1, 2, 3], [-1]], [-2]))
        assert(solver.failed_literals?([[1, -2, 3], [4], [-1, -4], [-1, -2, -3, 4]], [1]))
        assert(!solver.failed_literals?([[1, -2, 3], [4], [-1, 4], [-1, -2, -3, 4]], [1]))
        assert(!solver.failed_literals?([[1, 2, 3], [2, 3, 4], [2, 3, -1]], [-2]))
        assert(solver.failed_literals?([[1, 2, 3], [2, 3, 4], [2, 3, -1]], [-2, -3]))
    end
    
    def test_solve_failed_literal
        solver = SatSolver.new([[1, -2, 3], [-1, 2, 3], [-4, 1], [-2, -3], [1, 3], [1, 2]])
        assert_equal([:unresolved, [1], [[2, 3], [-2, -3]]], solver.solve_without_search({:failed_literal => true}))
        
        solver.set_formula([[-1, -2], [2, 3], [-3, 2], [-1, -2, -3]])
        assert_equal([:sat, [-1, 2]], solver.solve_without_search({:failed_literal => true}))
        
        solver.set_formula([[1, -2], [1, 3], [-3, 2], [-1]])
        assert_equal([:unsat], solver.solve_without_search({:failed_literal => true}))
        
        solver.set_formula([[1, 4], [2, 6], [-2, -5], [3, 2], [-3, -6], [3, 5, 6, 1], [4, 6], [5, 1]])
        assert_equal([:unresolved, [-5, 1, 2], [[-3, -6], [4, 6]]], solver.solve_without_search({:failed_literal => true}))
    end
    
    def test_solve_binary_failed_literal
    end
    
    def test_solve_affirmative_negative
        solver = SatSolver.new([[1, -2, 3], [-1, 2, 3], [-4, 1], [-2, -3], [1, 3], [1, 2]])
        assert_equal([:unresolved, [1, -4], [[2, 3], [-2, -3]]], solver.solve_without_search({:affirmative_negative => true, :failed_literal => true}))
    end
    
    def test_with_search
        solver = SatSolver.new([[1, -2, 3], [-1, 2, 3], [-4, 1], [-2, -3], [1, 3], [1, 2]])
        assert_equal([:sat, [1, 2, -3]], solver.solve_with_search({:failed_literal => true}))
        
        solver.set_formula([[9, 8], [-8, 11], [1, 11], [2, 12, 10, -11], [8, 7, 6]])
        assert_equal([:sat, [9, 8, 11, 2]], solver.solve_with_search)
    end
        
    def test_generator
        srand(2552)
        
        assert_equal(    [[12, -9], [-14, 5, -2], [-14, -10, -15], [-9, -1], [14, 10, 8], [-5, -12, -14]],
                        SatUtils.generate_formula(15, 6, 3))        
        assert_equal(     [[-8, 5, 1], [3, -5, 6], [3, -1], [-1, -6, -5], [3, 8], [-4, 8, -3], [-4, 1], [-7, -3], [7, 6], [4, -8]],
                        SatUtils.generate_formula(8, 10, 3))
    end
                    
    # playground
    def test_test 
        #~ srand(55)
        #~ form = SatUtils.generate_formula(15, 6, 4)
        #~ p form
        
        #~ solver = SatSolver.new([[-2, -3], [2, 3].])
        #~ solver.debug = true
        #~ p solver.solve_with_search({:failed_literal => true})
    end
    
end

