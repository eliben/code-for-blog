import unittest

#~ import rd_parser_ebnf as rd_parser
import rd_parser_infix_expr as rd_parser


class TestCalcParser(unittest.TestCase):
    def setUp(self):
        self.calc_parser = rd_parser.CalcParser()

    def calc(self, calc_stmt):
        self.calc_parser.calc(calc_stmt)

    def assertResult(self, calc_stmt, result):
        self.assertEqual(self.calc_parser.calc(calc_stmt), result)

    def assertParseError(self, calc_stmt):
        self.assertRaises(rd_parser.ParseError,
            self.calc_parser.calc, calc_stmt)

    def test_basics(self):
        self.assertResult('5', 5)
        self.assertResult('2 * (2 - 2)', 0)
        self.assertResult('2 + 7', 9)
        self.assertResult('22 / 11', 2)
        self.assertResult('2 + -3', -1)
        self.assertResult('2 ** -3', 0.125)
        self.assertResult('2 + -6 ** 2', 38)
        self.assertResult('2 + 7 * -3', -19)
        self.assertResult('2 ** 3 ** 2 + 7', 519)
        self.assertResult('9991929 > 881828', True)
        self.assertResult('9991929 >= 881828', True)
        self.assertResult('9991929 < 881828', False)
        self.assertResult('9991929 <= 881828', False)
        self.assertResult('9991929 != 881828', True)
        self.assertResult('9991929 == 881828', False)

    def test_bitwise(self):
        self.assertResult('2 << 2', 8)
        self.assertResult('1024 >> 3', 128)
        self.assertResult('15 & 12', 12)
        self.assertResult('15 | 12', 15)
        self.assertResult('5 ^ 2', 7)
        self.assertResult('5 ^ 16 & 12 + 11 | 13', 29)
        self.assertResult('2 & 3 ^ (4 | 1) * 11 | 1002 & 4', 53)

    def test_vars(self):
        self.calc('set joe = 15')
        self.calc('set mxa = 993 - 998')
        self.assertResult('joe + mxa * 2', 5)

        self.calc('set joe = 10')
        self.calc('set kkkkkk = joe + -mxa')
        self.assertResult('kkkkkk', 15)

    def test_statements(self):
        self.calc('set x = 4')
        self.assertResult('if x == 4 then 10 else 15', 10)
        self.assertResult('if x != 4 then 10 else 15', 15)
        self.assertResult('if x != 4 then 10', None)
        self.assertResult('if x == 4 then 10', 10)

        self.calc('set p = 1')
        self.calc('if 1 == 0 then set p = 12 else set p = 16')
        self.assertResult('p', 16)
        self.calc('if 0 == 0 then set p = 12 else set p = 16')
        self.assertResult('p', 12)
        self.calc('if 5 >= 5 then set p = p * 2 else set p = 0')
        self.assertResult('p', 24)
        self.calc('if 5 > 5 then set p = p * 2 else set p = 0')
        self.assertResult('p', 0)

    def test_long(self):
        self.calc('set joe = 2 - 1 - 1')           # 0
        self.calc('set mar = joe + 2 ** 4 * -3')   # -48
        self.calc('set pie = 2 ** 3 ** 2')         # 512
        self.calc('if joe != 0 then set pie = 3')  # pie stays 512
        self.calc('if 1 == 1 then set k = 10 else set k = 20') # 10
        self.calc('if k > 20 then set k = 12')     # k stays 10
        self.calc('if k <= 11 then set t = 0 else set t = 2') # 0
        self.assertResult('pie - (k * -mar) + k + t', 42)

    def test_errors(self):
        self.assertParseError('2 + ')
        self.assertParseError('apchi')
        self.assertParseError('1 + 2 12')
        self.assertParseError('(1 + 2')
        self.assertParseError('p + 1')
        self.assertParseError('if 1 == 2 the')


if __name__ == '__main__':
    unittest.main()
