# Folds in Python.
#
# Tested with Python 3.4+ (though I expect it to work with 2.x too).
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
def sum(seq):
    if not seq:
        return 0
    else:
        return seq[0] + sum(seq[1:])


def product(seq):
    if not seq:
        return 1
    else:
        return seq[0] * product(seq[1:])


def double(seq):
    if not seq:
        return []
    else:
        return [seq[0] * 2] + double(seq[1:])


def map(mapf, seq):
    if not seq:
        return []
    else:
        return [mapf(seq[0])] + map(mapf, seq[1:])


def filter(predicate, seq):
    if not seq:
        return []
    else:
        maybeitem = [seq[0]] if predicate(seq[0]) else []
        return maybeitem + filter(predicate, seq[1:])


def transform(init, mapping, combination, seq):
    if not seq:
        return init
    else:
        return combination(mapping(seq[0]),
                           transform(init, mapping, combination, seq[1:]))


def product_with_transform(seq):
    return transform(1, lambda x: x, lambda a, b: a * b, seq)


def product_with_foldr(seq):
    return foldr(lambda seqval, acc: seqval * acc, 1, seq)


def double_with_transform(seq):
    return transform([], lambda x: [x * 2], lambda a, b: a + b, seq)


def foldr(func, init, seq):
    if not seq:
        return init
    else:
        return func(seq[0], foldr(func, init, seq[1:]))


def foldrloop(func, init, seq):
    acc = init
    for v in reversed(seq):
        acc = func(v, acc)
    return acc


def sum_with_foldr(seq):
    return foldr(lambda seqval, acc: seqval + acc, 0, seq)


def sum_with_foldrloop(seq):
    return foldrloop(lambda seqval, acc: seqval + acc, 0, seq)


def double_with_foldr(seq):
    return foldr(lambda seqval, acc: [seqval * 2] + acc, [], seq)


def double_with_foldrloop(seq):
    return foldrloop(lambda seqval, acc: [seqval * 2] + acc, [], seq)


def map_with_foldr(mapf, seq):
    return foldr(lambda seqval, acc: [mapf(seqval)] + acc, [], seq)


def filter_with_foldr(predicate, seq):
    def reducer(seqval, acc):
        if predicate(seqval):
            return [seqval] + acc
        else:
            return acc
    return foldr(reducer, [], seq)


# Implements short-circuit 'and' on the sequence; in Python lists are eager, so
# the whole list will be accessed anyway. However, if seq is lazy, the the rest
# shouldn't be evaluated after False is found.
def shortcircuit_with_foldr(seq):
    def reducer(seqval, acc):
        if not seqval:
            return False
        else:
            return acc
    return foldr(reducer, True, seq)


def reverse_with_foldr(seq):
    return foldr(lambda seqval, acc: acc + [seqval], [], seq)


def foldl(func, init, seq):
    if not seq:
        return init
    else:
        return foldl(func, func(init, seq[0]), seq[1:])


def product_with_foldl(seq):
    return foldl(lambda acc, seqval: acc * seqval, 1, seq)


def digits2num_with_foldl(seq):
    return foldl(lambda acc, seqval: acc * 10 + seqval, 0, seq)


identity = lambda x: x


# Function composition with foldr; fseq is a sequence of unary functions.
# Returns a unary function that is a composition of all the functions in fseq,
# from left to right.
def fcompose_with_foldr(fseq):
    return foldr(lambda seqval, acc: lambda x: seqval(acc(x)), identity, fseq)


def productl_with_foldr(seq):
    composed = foldr(
                lambda seqval, acc: lambda n: acc(n * seqval),
                identity,
                seq)
    return composed(1)


def digits2num_with_foldr(seq):
    composed = foldr(
                lambda seqval, acc: lambda n: acc(n * 10 + seqval),
                identity,
                seq)
    return composed(0)


def foldl_with_foldr(func, init, seq):
    composed = foldr(
                lambda seqval, acc: lambda n: acc(func(n, seqval)),
                identity,
                seq)
    return composed(init)


#------------------------------ Testing --------------------------------#

import unittest


class TestStuff(unittest.TestCase):
    def test_sum(self):
        self.assertEqual(sum([1, 2, 3, 4]), 10)
        self.assertEqual(sum([12]), 12)
        self.assertEqual(sum([]), 0)

        self.assertEqual(sum_with_foldr([1, 2, 3, 4]), 10)
        self.assertEqual(sum_with_foldr([12]), 12)
        self.assertEqual(sum_with_foldr([]), 0)

        self.assertEqual(sum_with_foldrloop([1, 2, 3, 4]), 10)
        self.assertEqual(sum_with_foldrloop([12]), 12)
        self.assertEqual(sum_with_foldrloop([]), 0)

    def test_product(self):
        self.assertEqual(product([1, 2, 3, 4]), 24)
        self.assertEqual(product([12]), 12)
        self.assertEqual(product([]), 1)

        self.assertEqual(product_with_transform([1, 2, 3, 4]), 24)
        self.assertEqual(product_with_transform([12]), 12)
        self.assertEqual(product_with_transform([]), 1)

        self.assertEqual(product_with_foldr([1, 2, 3, 4]), 24)
        self.assertEqual(product_with_foldr([2, 4, 6, 8]), 384)
        self.assertEqual(product_with_foldr([12]), 12)
        self.assertEqual(product_with_foldr([]), 1)

        self.assertEqual(product_with_foldl([1, 2, 3, 4]), 24)
        self.assertEqual(product_with_foldl([2, 4, 6, 8]), 384)
        self.assertEqual(product_with_foldl([12]), 12)
        self.assertEqual(product_with_foldl([]), 1)

        self.assertEqual(productl_with_foldr([1, 2, 3, 4]), 24)
        self.assertEqual(productl_with_foldr([2, 4, 6, 8]), 384)
        self.assertEqual(productl_with_foldr([12]), 12)
        self.assertEqual(productl_with_foldr([]), 1)

        pfl = lambda seq: \
                foldl_with_foldr(lambda acc, seqval: acc * seqval, 1, seq)
        self.assertEqual(pfl([1, 2, 3, 4]), 24)
        self.assertEqual(pfl([2, 4, 6, 8]), 384)
        self.assertEqual(pfl([12]), 12)
        self.assertEqual(pfl([]), 1)


    def test_double(self):
        self.assertEqual(double([1, 2, 3, 4]), [2, 4, 6, 8])
        self.assertEqual(double([12]), [24])
        self.assertEqual(double([]), [])

        self.assertEqual(double_with_transform([1, 2, 3, 4]), [2, 4, 6, 8])
        self.assertEqual(double_with_transform([12]), [24])
        self.assertEqual(double_with_transform([]), [])

        self.assertEqual(double_with_foldr([1, 2, 3, 4]), [2, 4, 6, 8])
        self.assertEqual(double_with_foldr([12]), [24])
        self.assertEqual(double_with_foldr([]), [])

        self.assertEqual(double_with_foldrloop([1, 2, 3, 4]), [2, 4, 6, 8])
        self.assertEqual(double_with_foldrloop([12]), [24])
        self.assertEqual(double_with_foldrloop([]), [])

    def test_map(self):
        self.assertEqual(map(len, ['i', 'foo', 'kk']), [1, 3, 2])
        self.assertEqual(map(len, ['carrot']), [6])
        self.assertEqual(map(len, []), [])

        self.assertEqual(map_with_foldr(len, ['i', 'foo', 'kk']), [1, 3, 2])
        self.assertEqual(map_with_foldr(len, ['carrot']), [6])
        self.assertEqual(map_with_foldr(len, []), [])

    def test_filter(self):
        isodd = lambda n: (n % 2) == 1
        self.assertEqual(filter(isodd, [2, 3, 4, 7]), [3, 7])
        self.assertEqual(filter(isodd, [2, 4, 7]), [7])
        self.assertEqual(filter(isodd, [2, 4]), [])
        self.assertEqual(filter(isodd, []), [])

        self.assertEqual(filter_with_foldr(isodd, [2, 3, 4, 7]), [3, 7])
        self.assertEqual(filter_with_foldr(isodd, [2, 4, 7]), [7])
        self.assertEqual(filter_with_foldr(isodd, [2, 4]), [])
        self.assertEqual(filter_with_foldr(isodd, []), [])

    def test_shortcircuit(self):
        self.assertEqual(shortcircuit_with_foldr([]), True)
        self.assertEqual(shortcircuit_with_foldr([True]), True)
        self.assertEqual(shortcircuit_with_foldr([True, True]), True)
        self.assertEqual(shortcircuit_with_foldr([True, True, True]), True)
        self.assertEqual(shortcircuit_with_foldr([False, True]), False)
        self.assertEqual(shortcircuit_with_foldr([True, False]), False)
        self.assertEqual(shortcircuit_with_foldr([True, False, True]), False)

    def test_reverse(self):
        self.assertEqual(reverse_with_foldr([1, 2, 3]), [3, 2, 1])
        self.assertEqual(reverse_with_foldr([1, 2]), [2, 1])
        self.assertEqual(reverse_with_foldr([2]), [2])
        self.assertEqual(reverse_with_foldr([]), [])

    def test_digits2num(self):
        self.assertEqual(digits2num_with_foldl([2, 3]), 23)
        self.assertEqual(digits2num_with_foldl([5, 2, 3]), 523)
        self.assertEqual(digits2num_with_foldl([3]), 3)
        self.assertEqual(digits2num_with_foldl([]), 0)

        self.assertEqual(digits2num_with_foldr([2, 3]), 23)
        self.assertEqual(digits2num_with_foldr([5, 2, 3]), 523)
        self.assertEqual(digits2num_with_foldr([3]), 3)
        self.assertEqual(digits2num_with_foldr([]), 0)

    def test_fcompose(self):
        fc1 = fcompose_with_foldr([lambda x: x+1, lambda x: x*7, lambda x: -x])
        self.assertEqual(fc1(8), -55)

        fc2 = fcompose_with_foldr([lambda x: x+1])
        self.assertEqual(fc2(8), 9)

        fcid = fcompose_with_foldr([])
        self.assertEqual(fcid(8), 8)


if __name__ == '__main__':
    unittest.main()
