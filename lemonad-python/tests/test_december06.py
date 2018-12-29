"""
Unit tests for December 06, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december06 import Solver


class TestDecember06(unittest.TestCase):
    def test_examplem(self):
        example_input = """
        1, 1
        1, 6
        8, 3
        3, 4
        5, 5
        8, 9
        """
        s = Solver(from_str=example_input)
        (one, two) = s.solve(limit=32)
        self.assertEqual(one, 17)
        self.assertEqual(two, 16)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december06.input").solve()
        self.assertEqual(one, 3871)
        self.assertEqual(two, 44667)


if __name__ == "__main__":
    unittest.main()
