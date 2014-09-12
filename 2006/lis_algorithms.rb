#
# Implementation of a few algorithms for the  Longest Increasing 
# Subsequence problem. See this Wikipedia entry for a clarification:
# http://en.wikipedia.org/wiki/Longest_increasing_subsequence_problem
#
# The algorithms are:
#
# 1) Using the the Longest _Common_ Subsequence algorithm on a sequence
#    with its sorted self: O(n^2)
# 2) Using a dynamic programming algorithm: O(n^2) but faster than (1)
#    because of better constants and less generality
# 3) Using a greedy algorithm: O(n*logn)
#
# There's also a comparison of the algorithms' performance with Benchmark,
# and a few methods to check the implementation's correctness
#
# Copyright (C) <2006>  Eli Bendersky
#
# This code is licensed with the LGPL
#
require 'pp'
require 'benchmark'


# Terminology: 
# A sequence is a list of objects arranged in a "linear" fashion, 
# such that the order of the members is well defined and significant. 
# Sequences are represented by Ruby Arrays in this code.
# 
# A subsequence of some sequence is a new sequence which is formed 
# from the original sequence by deleting some of the elements without 
# disturbing the relative positions of the remaining elements. An important
# point to be made is that a subsequence is not necessarily made of 
# contiguous elements of the original sequence. For instance, given the
# sequence: [1, 2, 3, 4, 5, 6]
# This is a subsequence: [2, 3, 4]
# This is also a subsequence: [2, 3, 6]
#


# Given a sequence, randomly shuffles it in-place
#
def shuffle_sequence!(a)
    (a.size - 1).downto(1) do |i|
        j = rand(i + 1)
        a[i], a[j] = a[j], a[i] if i != j
    end
end


# Given n creates a random sequence of the numbers 1 .. n
#
def make_random_sequence(n)
	seq = (1 .. n).to_a
	shuffle_sequence!(seq)
	return seq
end


# Given a sequence, tells wheter it is strictly increasing.
#
def increasing_sequence?(seq)
	seq.inject {|mem, x| return false if mem >= x; x}
	true
end


# Is seq a subsequence of other_seq
#
def is_subsequence_of?(seq, other_seq)
	i = 0	
	seq.each do |x|
		while other_seq[i] != x
			i += 1
			return false if (i >= other_seq.length)
		end
	end
	
	return true
end


# Given two arrays, finds their longest common subsequence using dynamic 
# programming
#
# Note: this solves the more general LCS problem.
# see http://en.wikipedia.org/wiki/Longest_common_subsequence
#
# Complexity: O(x.size * y.size)
#
def find_lcs_dynamic(x, y)
	# The algorithm works on 1-based arrays,
	# where n elements are at indices 1 .. n
	#
	m = x.length
	n = y.length
	
	x = [0, *x]
	y = [0, *y]
	
	# 2D tables (m + 1) x (n + 1)
	c = Array.new(m + 1) {Array.new(n + 1) {0}}
	b = Array.new(m + 1) {Array.new(n + 1) {0}}
	
	# construct the tables
	1.upto(m) do |i|
		1.upto(n) do |j| 
			if x[i] == y[j]
				c[i][j] = c[i - 1][j - 1] + 1
				b[i][j] = "UP-LEFT"
			elsif c[i - 1][j] >= c[i][j - 1]
				c[i][j] = c[i - 1][j]
				b[i][j] = "UP"
			else
				c[i][j] = c[i][j - 1]
				b[i][j] = "LEFT"
			end
		end
	end
	
	# use the b table to reconstruct the LCS
	lcs = []
	i = m
	j = n
	loop do
		#~ puts "#{i} #{j} -> #{b[i][j]}"
		
		if i == 0 or j == 0
			break
		elsif b[i][j] == "UP-LEFT"
			lcs.unshift x[i]
			i -= 1
			j -= 1
		elsif b[i][j] == "UP"
			i -= 1
		elsif b[i][j] == "LEFT"
			j -= 1
		else
			raise "Unreachable" 
		end
	end
	
	return lcs
end


# Given an array, finds its longest increasing
# subsequence using dynamic programming
# Complexity: O(n^2)
#
def find_lis_dynamic(x)
	n = x.length
	
	best = Array.new(n) {1}
	max = 0
	max_index = 0
	
	# build the lis-length array, which at any index
	# contains the length of the lis so far. Keep 
	# track of the longest LIS with max / max_index
	#
	0.upto(n - 1) do |i|
		0.upto(i - 1) do |j|
			if x[i] > x[j] && best[i] < best[j] + 1
				best[i] = best[j] + 1
				
				if best[i] > max
					max = best[i]
					max_index = i 
				end
			end
		end
	end
	
	# reconstruct the lis
	lis = []
	k = max + 1
	
	max_index.downto(0) do |i|
		if best[i] == k - 1
			lis.unshift(x[i])
			k = best[i]
		end
	end
	
	return lis
end


# Given an array, finds its longest increasing subsequence using an efficient 
# greedy algorithm - MLAS (http://home.tiac.net/~cri/2001/mlas.html)
# Complexity: O(n*log(n))
#
def find_lis_greedy(seq)
	n = seq.length
	terminals = [0] * (n + 1)
	backptrs = [-1] + [0] * (n - 1)
	
	lis = []
	n_lis = 1
	
	1.upto(n - 1) do |i|
		low = 1
		high = n_lis
		
		while low <= high
			mid = (low + high) / 2
			if seq[i] <= seq[terminals[mid]] 
				high = mid - 1
			else
				low = mid + 1
			end
		end

		terminals[low] = i
		if low <= 1 
			backptrs[i] = -1
		else
			backptrs[i] = terminals[low - 1]
		end
		
		n_lis += 1 if low > n_lis
	end
	
	lis[n_lis - 1] = seq[terminals[n_lis]]
	temp = terminals[n_lis]
	
	(n_lis - 2).downto(0) do |i|
		temp = backptrs[temp]
		lis[i] = seq[temp]
	end
	
	return lis
end

#--------------------------------------------------------------------------------------------------#

ntimes = 5000

Benchmark.bmbm(10) do |x|
	x.report("lcs") do
		ntimes.times do 
			seq = make_random_sequence(20)
			mylcs = find_lcs_dynamic(seq, seq.sort)
			
			raise "Not increasing" unless increasing_sequence?(mylcs)
			raise "Not subsequence" unless is_subsequence_of?(mylcs, seq)
		end
	end
		
	x.report("lis_dyn") do
		ntimes.times do
			seq = make_random_sequence(20)
			mylis = find_lis_dynamic(seq)
			
			raise "Not increasing" unless increasing_sequence?(mylis)
			raise "Not subsequence" unless is_subsequence_of?(mylis, seq)
		end
	end
	
	x.report("lis_gr") do
		ntimes.times do
			seq = make_random_sequence(20)
			mylis = find_lis_greedy(seq)
			
			raise "Not increasing" unless increasing_sequence?(mylis)
			raise "Not subsequence" unless is_subsequence_of?(mylis, seq)
		end
	end
end
