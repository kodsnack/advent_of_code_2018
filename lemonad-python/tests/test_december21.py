"""
Unit tests for December 21, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december21 import Solver


class TestDecember21(unittest.TestCase):
    def test_solution(self):
        (one, two) = Solver(from_file="input/december21.input").solve()
        self.assertEqual(one, 13522479)
        self.assertEqual(two, 14626276)


if __name__ == "__main__":
    unittest.main()
