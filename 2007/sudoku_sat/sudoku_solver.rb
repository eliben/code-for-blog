##
#
# A SAT based Sudoku solver. Accepts Sudoku boards and solves them by generating constraints
# as CNF formulas and solving them with the SAT solver sat_solver.rb
#
# Loosely based on the article "Sudoku as a SAT problem" by Ines Lynce and Joel Ouaknine.
# Note: the failed_literal method with extended encoding is enough to solve the hardest
# Sudoku puzzles (the ones with only 17 assigned cells). 
# 
# by Eli Bendersky. License: LGPL
#
##
#
require 'benchmark'
require 'pp'
require 'eliben_utils'
require 'sat_solver'


#
# Note: this solver is tailored for the standard 9x9 Sudoku. 
#
# The internal SudokuSolver board representation:
#
# An array of nine 9-element arrays. Each element is a number in the range 0..9,
# where 1..9 means an assigned cell and 0 means an unassigned cell.
#
# String representations of Sudoku puzzles:
#
# "Canonical": a string of 81 numbers 0-9. The first 9 are the first row, the
# next 9 are the second row and so on. The number 0 means an empty (unassigned)
# cell, and any other number (1..9) means a cell assigned to this number.
# For example:
#  002005079105003000000000600010400900090000080004009010009000000000100306680300400
#
# "Pretty": a ascii-art representation of the board. The board shown in canonical
# representation above looks like this in pretty representation:
#
# +-------+-------+-------+
# | _ _ 2 | _ _ 5 | _ 7 9 |
# | 1 _ 5 | _ _ 3 | _ _ _ |
# | _ _ _ | _ _ _ | 6 _ _ |
# +-------+-------+-------+
# | _ 1 _ | 4 _ _ | 9 _ _ |
# | _ 9 _ | _ _ _ | _ 8 _ |
# | _ _ 4 | _ _ 9 | _ 1 _ |
# +-------+-------+-------+
# | _ _ 9 | _ _ _ | _ _ _ |
# | _ _ _ | 1 _ _ | 3 _ 6 |
# | 6 8 _ | 3 _ _ | 4 _ _ |
# +-------+-------+-------+
#
#
class SudokuSolver
    
    attr_reader :board
    
    # Initializes the solver object. It is possible to initialize directly from a string
    # containing the board. In this case, the type of the representation must be specified
    # (:pretty or :canonical)
    #
    def initialize(str = nil, type = nil)
        @board = []
        
        unless str.nil?
            fail "No board type specified" if type.nil?
            
            case type
                when :canonical
                    load_from_canonical(str)
                when :pretty
                    load_from_pretty(str)
                else
                    fail "Unknown type specified"
            end
        end
    end
    
    # Loads a board from a "pretty" board representation
    #
    def load_from_pretty(str)
        @board = []
        
        str.each_line do |line|
            next unless line =~ /[\d_]/
            @board << line.scan(/[\d_]/).map {|n| Integer(n) rescue 0}
            fail "Found a row with length #{@board.last.length}" unless @board.last.length == 9
        end
        
        fail "Expected 9 rows" unless @board.length == 9
    end
    
    # Loads a board from a "canonical" board representation
    #
    def load_from_canonical(str)
        fail "Expected a string of 81 digits" unless str =~ /^\d{81}$/;
        
        @board = []
        str.scan(/\d{9}/) do |row|
            @board << row.split('').mapf(:to_i)
        end
    end
    
    # Returns the canonical representation of the board as a string
    #
    def to_s_canonical
        board.join('')
    end
    
    # Returns the pretty representation of the board as a string
    #
    def to_s_pretty
        str = ""
        
        border = '+-------+-------+-------+'
        separator = '|'
        
        (0 .. 8).each do |row|
            str += (border + "\n") if row % 3 == 0
            
            (0 .. 8).each do |col|
                str += (separator + ' ') if col % 3 == 0
                str += (@board[row][col].to_s + ' ')
            end
            
            str += (separator + "\n")
        end
        
        str += (border + "\n")
        str.gsub!('0', '_')
        return str
    end

    # Creates the CNF formula that represents this Sudoku board (combines the generic
    # constraints for Sudoku with the board-specific cell assignments)
    #
    def make_cnf_formula(encoding)
        cnf_formula = []
        
        # First create a generic formula which isn't specific to our board
        #
        case encoding
            when :minimal
                cnf_formula = SudokuSolver.minimal_encoding_formula
            when :extended
                cnf_formula = SudokuSolver.extended_encoding_formula
            else
                fail "Unknown encoding #{encoding}"
        end
        
        # Now add the unit clauses that result from the assigned cells in our board
        #
        (0 .. 8).each do |row|
            (0 .. 8).each do |col|
                if board[row][col] > 0
                    cnf_formula.unshift([SudokuSolver.cell2variable(row, col, board[row][col])])
                end
            end
        end
        
        cnf_formula        
    end
    
    # Updates the internal Sudoku board from the SAT solution (a list of assigned SAT vars)
    #
    def update_board_from_solution(vars)
        vars.each do |var|
            if var > 0
                row, col, val = SudokuSolver.variable2cell(var)
                
                unless ((0 .. 8) === row and (0 .. 8) === col and (1 .. 9) === val)
                    fail "Invalid var #{var} in solution"
                end
                
                @board[row][col] = val
            end
        end
    end
    
    # Checks if the internal Sudoku board is solved. A board is solved if all its cells are
    # filled and it's valid (all the Sudoku constraints are fulfilled)
    #
    def board_solved?
        # Check that all the cells are filled with values
        #
        (0 .. 8).each do |row|
            (0 .. 8).each do |col|
                return false unless board[row][col] > 0
            end
        end
        
        # Check column constraints
        #
        @board.each do |column|
            return false unless column.uniq.size == 9
        end
        
        # Check row constraints
        #
        @board.transpose.each do |column|
            return false unless column.uniq.size == 9
        end
        
        # Check box constraints
        #
        (0 .. 2).each do |box_x|
            (0 .. 2).each do |box_y|
                box_vals = []
                
                (0 .. 2).each do |i|
                    (0 .. 2).each do |j|
                        box_vals << @board[3 * box_x + i][3 * box_y + j]
                    end
                end
                
                return false unless box_vals.uniq.size == 9
            end
        end
        
        true
    end

    # Attempts to solve the board using a SAT solver. Returns true if the board has a solution,
    # false otherwise.
    #
    def solve(encoding)
        cnf_formula = make_cnf_formula(encoding)
        
        # Try solving the formula using the SAT solver
        #
        sat_solver = SatSolver.new(cnf_formula)
        result = sat_solver.solve_without_search(:failed_literal => true)
               
        if result[0] == :sat
            update_board_from_solution(result[1])
            
            if board_solved?
                return true
            else
                fail "Error: SAT Solution invalid"
            end
        else
            return false
        end
    end
    
    # Cells are encoded to CNF variables as follows:
    #
    # board(row, col) = value --> var_n = 100 * x + 10 * y + z
    #
    # row and col run from 0 to 8 (indices into @board) and value is between 1 and 9.
    #
    def SudokuSolver.cell2variable(row, col, value)
        100 * row + 10 * col + value
    end
    
    def SudokuSolver.variable2cell(var)
        [var / 100, (var % 100) / 10, var % 10]
    end
    
    # Creates a generic minimal encoding CNF formula for Sudoku problems. 
    #
    def SudokuSolver.minimal_encoding_formula
        formula = []
        
        # There is at least one number in each cell. For each cell in the board
        # create a clause that says that at least one of 1 .. 9 is there
        #
        (0 .. 8).each do |row|
            (0 .. 8).each do |col|
                clause = (1 .. 9).map {|val| cell2variable(row, col, val)}
                formula << clause
            end
        end
        
        # Each number appears at most once in each column. For each column
        # and value create a binary clause that says that there can be
        # no two different cells with the same value within the same row.
        #
        (0 .. 8).each do |col|
            (1 .. 9).each do |val|
                (0 .. 7).each do |row|
                    (row + 1 .. 8).each do |other_row|
                        formula << [-cell2variable(row, col, val), -cell2variable(other_row, col, val)]
                    end
                end
            end
        end
        
        # Each number appears at most once in each row.
        #
        (0 .. 8).each do |row|
            (1 .. 9).each do |val|
                (0 .. 7).each do |col|
                    (col + 1 .. 8).each do |other_col|
                        formula << [-cell2variable(row, col, val), -cell2variable(row, other_col, val)]
                    end
                end
            end
        end

        # Each number appears at most once in each major 3x3 box. Boxes are traversed
        # using 3x+i notation, such that x is the box and i the index in the box
        # (in one direction, either row, or column). So:
        # (3x+i, 3y+j) - with x and y kept constant and i, j moving from 1 to 3 we
        # traverse some box. When x or y are changed, we move to another box.
        #
        (1 .. 9).each do |val|
            (0 .. 2).each do |box_x|
                (0 .. 2).each do |box_y|
                    (0 .. 2).each do |i|
                        (0 .. 2).each do |j|
                            (i + 1 .. 2).each do |k|
                                (0 .. 2).each do |l|
                                    formula << [-cell2variable(3 * box_x + i, 3 * box_y + j, val),
                                                -cell2variable(3 * box_x + k, 3 * box_y + l, val)]
                                end
                            end
                        end
                    end
                end
            end
        end
        
        formula
    end
    
    # Creates an extended encoding CNF formula for Sudoku problems, by adding constraints
    # to the minimal encoding.
    #
    def SudokuSolver.extended_encoding_formula
        formula = minimal_encoding_formula()
        
        # There is at most one number in each cell
        #
        (0 .. 8).each do |row|
            (0 .. 8).each do |col|
                (1 .. 8).each do |val|
                    (val + 1 .. 9).each do |other_val|
                        formula << [-cell2variable(row, col, val), -cell2variable(row, col, other_val)]
                    end
                end
            end
        end
        
        # Each number appears at least once in each row
        #
        (1 .. 9).each do |val|
            (0 .. 8).each do |col|
                clause = (0 .. 8).map {|row| cell2variable(row, col, val)}
                formula << clause
            end
        end
        
        # Each number appears at least once in each column
        #
        (1 .. 9).each do |val|
            (0 .. 8).each do |row|
                clause = (0 .. 8).map {|col| cell2variable(row, col, val)}
            end
        end
        
        # Each number appears at least once in each major 3x3 box
        #
        # See the comments in minimal_encoding_formula for explanation of how box
        # indices are computed
        #
        (0 .. 2).each do |box_x|
            (0 .. 2).each do |box_y|
                (1 .. 9).each do |val|
                    clause = []
                    
                    (0 .. 2).each do |i|
                        (0 .. 2).each do |j|
                            clause << cell2variable(3 * box_x + i, 3 * box_y + j, val)
                        end
                    end
                    
                    formula << clause
                end
            end
        end
        
        formula
    end
end

