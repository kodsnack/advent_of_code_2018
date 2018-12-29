"""
Unit tests for December 08, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december08 import Solver


class TestDecember08(unittest.TestCase):
    def test_example(self):
        s = Solver(from_str="2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
        (one, two) = s.solve()
        self.assertEqual(one, 138)
        self.assertEqual(two, 66)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december08.input").solve()
        self.assertEqual(one, 40309)
        self.assertEqual(two, 28779)


if __name__ == "__main__":
    unittest.main()
