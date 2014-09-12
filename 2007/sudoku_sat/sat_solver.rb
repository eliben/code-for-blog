##
#
# A simple SAT Solver, based on the DPLL algorithm. Supports solving without search (inferential
# only) and with search, a several inference techniques.
#
# by Eli Bendersky. License: LGPL
#
##
#
# Terminology:
#
# * The symbols ~, + and * denote negation (NOT), disjunction (OR) and conjunction (AND), 
#   respectively.
#
# * A literal is either some variable v or its negation ~v. Literals are represented by
#   numbers: if we represent v by the number n then n means v and -n means ~v.
#
# * A clause is a disjunction of literals. It is represented by an Array. For example, 
#   if we use the numbers 1 .. n to represent variables v1 .. vn, then:
#   [6, -5, 8, 1, 2, -7] is the clause (v6 + ~v5 + v8 + v1 + v2 + ~v7)
#
# * A CNF formula is a conjunction of disjunctions. It is represented by an Array of 
#   clauses, i.e. an Array of Arrays.
#
# * An assignment is an Array of variables that were 'chosen'. For instance: [-1, 2, 13]
#   means that variable v1 is assigned FALSE and variables v2 and v13 are assigned TRUE.
#   A satisfying assignment is an assignment to all variables that yields a TRUE formula.
#
require 'set'
require 'pp'
require 'eliben_utils'


module SatUtils
    
    # A deep duplication of a formula
    #
    def SatUtils.dup_formula(formula)
        formula.map {|clause| clause.dup}
    end

    # Returns an Array of all the variables that appear in formula, either in positive
    # or negative form. The vars are always returned positive
    #
    def SatUtils.all_vars_in_formula(formula)
        set = Set.new
        
        formula.each do |clause|
            clause.each do |literal|
                var = literal.abs
                set.add(var)
            end
        end
        
        return set.to_a
    end
    
    # Given a formula and a literal, reduces the literal from the formula: 
    #  - Clauses containing the literal are removed (become TRUE)
    #  - Literal negations inside other clauses are removed
    #  - Other clauses are not modified
    #
    # Note: this function is destructive for formula
    #
    def SatUtils.reduce_literal(formula, literal)
        formula.delete_if {|clause| clause.include?(literal)}
        
        formula.each do |clause|
            clause.delete(-literal)
        end
    end
    
    # Looks for a unit clause in the given formula, and returns the first one found.
    # If the formula contains no unit clauses, nil is returned
    #
    def SatUtils.find_unit_clause(formula)
        return formula.find {|clause| clause.length == 1}
    end
    
    # Checks if the formula has a contradiction (an empty clause)
    #
    def SatUtils.has_contradiction?(formula)
        return formula.any? {|clause| clause.length == 0}
    end
    
    # Runs the 'unit propagation' inference on the given formula, by iteratively
    # applying unit clause removal. Returns the modified formula and the literals
    # reduced.
    #
    def SatUtils.unit_propagation(formula)
        vars = []
        new_formula = dup_formula(formula)
        
        while unit_clause = find_unit_clause(new_formula) do
            #puts "up reducing clause: #{unit_clause}, time: #{Time.now.to_f}"
            reduce_literal(new_formula, unit_clause[0])
            #puts "#{new_formula.size} clauses left, time: #{Time.now.to_f}"
            vars << unit_clause[0]
        end
        
        return [new_formula, vars]
    end
    
    # Generates random CNF formulas with the given parameters:
    #  * nvars - from how many variables to choose literals
    #  * nclauses - amount of clauses in the formula
    #  * avg_lits_per_clause - average amount of literals in each clause. Given avg, the
    #    amount is a uniformly distributed random number between 2 and 2 + (avg - 2) * 2, inclusive
    #
    # This method does not check its arguments thoroughly, so beware of providing illogical 
    # parameters.
    # The clauses will not have duplicate variables, that is for each clause C, if variable
    # V is in the clause, it appears there only once and -V doesn't appear.
    #
    def SatUtils.generate_formula(nvars, nclauses, avg_lits_per_clause)
        vars = (1 .. nvars).to_a
        formula = []
        
        nclauses.times do 
            num_lits = rand((avg_lits_per_clause - 2) * 2).to_i + 2            
            clause = []
            
            num_lits.times do 
                lit = vars.rand
                redo if clause.include?(lit) or clause.include?(-lit)
                clause.push(rand(2) == 0 ? lit : -lit)
            end
            
            formula.push(clause)
        end
        
        return formula
    end
    
end


class SatSolver

    attr_reader :formula, :formula_vars
    attr_accessor :debug

    # formula must be an Array of Arrays of Integers
    #
    def initialize(formula = [])
        set_formula(formula)
        @debug = false
    end
    
    # formula must be an Array of Arrays of Integers
    #
    def set_formula(formula)
        @formula = formula
        @formula_vars = SatUtils.all_vars_in_formula(formula)
    end
    
    # source can be anything that responds to :each_line (String, IO, StringIO, File...)
    # We expect the formula to be given with a single clause on each line, the clause 
    # being a list of whitespace-separated numbers. For example these contents:
    #   1 -2 3
    #   4 -1
    #
    # Imply the following formula (in internal format): [[1, -2, 3], [4, -1]].
    # Raises ArgumentError if source doesn't respond to :each_line or if the formula
    # is malformed.
    #
    def read_formula(source)
        raise ArgumentError, "expecting argument to respond to :each_line" unless source.respond_to? :each_line
        @formula = []
        
        source.each_line do |line|
            @formula += [line.split.mapf(:to_i)]
        end
        
        @formula_vars = SatUtils.all_vars_in_formula(formula)
    end    
    
    # Given a formula and an array of literals, tries to see if a contradiction is
    # reached when these literals are assigned.
    #
    def failed_literals?(formula, literals)
        literals_clauses = literals.zip
        new_formula = formula + literals_clauses
        return SatUtils.has_contradiction?(SatUtils.unit_propagation(new_formula)[0])
    end
    
    # Given a formula and a list of variables to pick from, tries to find a failed literal for
    # the formula (all variables are examined in their positive and negative forms).
    # Returns the first failed literal it finds. nil is returned if none were found.
    #
    def find_failed_literal(formula, vars)
        vars.each do |var|
            [var, -var].each do |literal|
                if failed_literals?(formula, [literal])
                    return literal
                end
            end
        end
        
        return nil
    end
    
    # Given a formula and a list of variables to pick from, tries to find a binary failed
    # literal. Returns the first pair it finds (as an Array of Integers). nil is returned
    # if none were found
    #
    def find_binary_failed_literal(formula, vars)
        vars.each_pair do |var1, var2|
            [var1, -var1].each do |lit1|
                [var2, -var2].each do |lit2|
                    if failed_literals?(formula, [lit1, lit2])
                        return [lit1, lit2]
                    end
                end
            end
        end
        
        return nil
    end
    
    # Given a formula, finds a list of variables according to the affirmative-negative
    # inference rule (Rule 2 in DPLL)
    #
    def find_affirmative_negative(formula)
        # First count to see which literals appear in the formula
        #
        literal_flags = {}        
        formula.each do |clause|
            clause.each do |literal|
                literal_flags[literal] = true
            end
        end
        
        # Add all literals whose negation doesn't appear in the formula
        #
        literals_to_reduce = []
        literal_flags.each_key do |literal|
            literals_to_reduce.push(literal) unless literal_flags.include?(-literal)
        end
        
        return literals_to_reduce
    end
    
    # Attempts to solve the formula without search (backtracking).
    # The known methods are: :failed_literal, :binary_failed_literal, :affirmative_negative
    #
    #   method - specifies which inference method(s) to use in addition to unit propagation. Expects this to be a Hash
    #            that contains 'method name => true' for each method desired.
    #
    # Returns:
    #   * [:sat, sat_vars] when the formula is satisfiable
    #   * [:unsat] when the formula is unsatisfiable (a contradiction was found)
    #   * [:unresolved, sat_vars, formula] when the formula is unresolved: the solver can't reach a conclusion. In this
    #     case, sat_vars is the partial assignment and formula is the reduced formula
    #                   
    #   When sat_vars is returned, it is an array of literals that were inferred, namely the satisfying assignment
    #   (or a partial satisfying assignment in case of :unresolved).
    #
    def solve_without_search(methods = {})
        use_failed_literal, use_binary_failed_literal, use_affirmative_negative = [false] * 3
        
        [:failed_literal, :binary_failed_literal, :affirmative_negative].each do |method|
            eval "use_#{method.to_s} = true" if methods[method]
        end
        
        formula = SatUtils.dup_formula(@formula.dup)
        vars = @formula_vars
        sats = []
        
        if @debug then puts "\n** solving formula: #{@formula.inspect}" end
        if @debug then puts "** formula_vars: #{@formula_vars.inspect}" end
        
        loop do
            return [:sat, sats] if formula.length == 0
            return [:unsat] if SatUtils.has_contradiction?(formula)
            changed = false
            
            reduced_formula, reduced_vars = SatUtils.unit_propagation(formula)
            
            if reduced_vars.empty? 
                changed = false
                #puts "UP nothing reduced"
            else
                changed = true
                sats += reduced_vars 
                formula = reduced_formula
                vars = vars - reduced_vars
                #puts "UP Reduced #{reduced_vars.size} vars. Clauses left: #{reduced_formula.size}"
            end
            
            if use_failed_literal
                if (literal = find_failed_literal(formula, vars))
                    formula.push([-literal])
                    changed = true
                    
                    #puts "Found failed literal: #{literal}"
                    if @debug then puts "**- failed literal: #{literal}" end
                end
            end
            
            if use_binary_failed_literal
                lit1, lit2 = find_binary_failed_literal(formula, vars)
                unless (lit1.nil?)
                    formula.push([-lit1, -lit2])
                    changed = true
                    
                    if @debug then puts "**- failed binary literal: #{lit1}, #{lit2}" end
                end
            end
            
            if use_affirmative_negative
                literals_to_reduce = find_affirmative_negative(formula)
                unless (literals_to_reduce.empty?)
                    formula += literals_to_reduce.zip
                    changed = true
                    
                    if @debug then puts "**- affirmative negative: #{literals_to_reduce.inspect}" end
                end
            end
            
            break unless changed
        end
        
        return [:unresolved, sats, formula]
    end
    
    def solve_with_search(methods = {}, formula = nil, sats = nil)
        # Top level call ?
        #
        if formula.nil?
            formula = @formula
            sats = []
        end
        
        status, sat_vars, formula = solve_without_search(methods)
        sats += sat_vars
        
        # If the formula is now resolved, we can return an answer
        #
        unless status == :unresolved
            return status, sats
        end
        
        # Otherwise, apply the search by picking a variable and trying to assign
        # it both ways.
        #
        search_var = pick_variable_for_search(formula)
        
        [search_var, -search_var].each do |literal|
            if @debug then print "**- trying assignment: #{literal} " end
            @formula = formula
            status, sats, new_formula = solve_with_search(methods, formula.push([literal]), sats)
            
            unless status == :unresolved                
                return status, sats
            end
        end
        
        # this is never reached #
        return [:error]
    end
    
    # Given a formula, picks a variable to apply search on (the search routine will 'guess' this
    # variable's value to deepen the search)
    #
    def pick_variable_for_search(formula)
        # For now a very rudimentary (and swift !) heuristic, that picks the first variable it finds
        #
        return formula[0][0].abs
    end
    
end

