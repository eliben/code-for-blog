# Unit tests for dot.py

import unittest
import dot


class TestDot(unittest.TestCase):

    def doTestDotFunc(self, dotfunc):
        self.assertEqual(dotfunc([], []), 0)
        self.assertEqual(dotfunc([2], [34]), 68)
        self.assertEqual(dotfunc([0, 0, 0], [0, 0, 0]), 0)
        self.assertEqual(dotfunc([1, 2], [3, 5]), 13)
        self.assertEqual(dotfunc([1, 2, 3], [4, 5, 6]), 32)
        self.assertEqual(dotfunc([1, 2, 3], [3, 2, 1]), 10)
        self.assertEqual(dotfunc([1, 2, 3, 5], [1, 2, 3, 6]), 44)

    def test_dotProductLoop(self):
        self.doTestDotFunc(dot.dotProductLoop)

    def test_dotProductZip(self):
        self.doTestDotFunc(dot.dotProductZip)

    def test_dotProductStarmap(self):
        self.doTestDotFunc(dot.dotProductStarmap)


if __name__ == "__main__":
    unittest.main()
