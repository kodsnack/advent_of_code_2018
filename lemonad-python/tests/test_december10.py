"""
Unit tests for December 10, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december10 import Solver


class TestDecember10(unittest.TestCase):
    def test_solution(self):
        two = Solver(from_file="input/december10.input").solve()
        self.assertEqual(two, 10355)


if __name__ == "__main__":
    unittest.main()
