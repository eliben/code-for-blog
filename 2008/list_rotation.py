# This file lists some Python functions for rotating a list 
# to the left. The emphasis is functions that don't use extra
# space.
# For an explanation, see 
# http://eli.thegreenplace.net/2008/08/29/space-efficient-list-rotation/ 
#
# Based on chapter 2 of "Programming Pearls" by Jon Bentley
#
# Eli Bendersky (http://eli.thegreenplace.net)
#


def rotate_naive(lst, dist):
    """ A 'naive' (space inefficient) rotation function.
        The slice operations create new lists.
    """
    lst[:] = lst[dist:len(lst)] + lst[0:dist]


def gcd(a, b):
    """ Greatest common divisor of a and b
        Using Euclid's algorithm
    """
    while b:
        a, b = b, a % b
    return a


def rotate_juggle(lst, dist):
    """ An iterative 'juggle' method
    """
    n = len(lst)
        
    for i in xrange(gcd(dist, n)):
        t = lst[i]
        j = i
        while 1:
            k = (j + dist) % n
            if k == i: break
            lst[j] = lst[k]
            j = k
        lst[j] = t


def sublist_swap(lst, a, b, m):
    """ Swaps (in-place) the elements:
        lst[a:a+m) with lst[b:b+m)
        Without using extra space.
        
        Assumes that all the indices point inside the list.
    """
    for i in xrange(m):
        lst[a + i], lst[b + i] = lst[b + i], lst[a + i]
        

def rotate_swap(lst, dist):
    """ A 'recursive' sub-list swapping method.        
    """
    n = len(lst)
    
    if dist == 0 or dist == n:
        return
    i = p = dist
    j = n - p
    
    while i != j:
        if i > j:
            sublist_swap(lst, p - i, p, j)
            i -= j
        else:
            sublist_swap(lst, p - i, p + j - i, i)
            j -= i
    
    sublist_swap(lst, p - i, p, i)


def sublist_reverse(lst, a, b):
    """ Reverses (in-place) the elements lst[a:b]
    """
    while b > a:
        lst[a], lst[b] = lst[b], lst[a]
        b -= 1
        a += 1


def rotate_reverse(lst, dist):
    """ Uses reversing to rotate the list.
    """
    n = len(lst)
    sublist_reverse(lst, 0, dist - 1)
    sublist_reverse(lst, dist, n - 1)
    sublist_reverse(lst, 0, n - 1)


import timeit

def benchmark():
    setup = """
from __main__ import rotate_naive, rotate_juggle, rotate_reverse, rotate_swap

lst = range(1000000)
dist = 100000
"""
    
    N = 10
    print "Naive:", timeit.Timer(stmt='rotate_naive(lst, dist)', setup=setup).timeit(N)
    print "Juggle:", timeit.Timer(stmt='rotate_juggle(lst, dist)', setup=setup).timeit(N)
    print "Swap:", timeit.Timer(stmt='rotate_swap(lst, dist)', setup=setup).timeit(N)
    print "Reverse:", timeit.Timer(stmt='rotate_reverse(lst, dist)', setup=setup).timeit(N)


import unittest

class TestListRotation(unittest.TestCase):
    def do_test_list(self, func, lst, dist, newlst):
        mylst = lst[:]
        func(mylst, dist)
        self.assertEqual(mylst, newlst)
    
    def run_tests(self, func):
        self.do_test_list(func, [], 0, [])
        self.do_test_list(func, [1], 0, [1])
        self.do_test_list(func, [1, 2], 0, [1, 2])
        self.do_test_list(func, [1, 2], 2, [1, 2])
        self.do_test_list(func, [1, 2], 1, [2, 1])
        self.do_test_list(func, [1, 2, 3], 1, [2, 3, 1])
        self.do_test_list(func, [1, 2, 3], 2, [3, 1, 2])
        
        N = 500
        for i in range(N):
            self.do_test_list(func, range(N), i, range(i, N) + range(i))
                
    def test_rotates(self):
        self.run_tests(rotate_naive)
        self.run_tests(rotate_juggle)
        self.run_tests(rotate_swap)
        self.run_tests(rotate_reverse)
    


if __name__ == '__main__':
    # Uncomment one of the following
    #~ unittest.main()
    benchmark()
