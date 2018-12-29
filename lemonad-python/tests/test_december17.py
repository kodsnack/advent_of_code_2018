"""
Unit tests for December 17, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december17 import Solver


class TestDecember17(unittest.TestCase):
    def test_sum_of_natural_numbers(self):
        example_input = """
            x=495, y=2..7
            y=7, x=495..501
            x=501, y=3..7
            x=498, y=2..4
            x=506, y=1..2
            x=498, y=10..13
            x=504, y=10..13
            y=13, x=498..504
        """
        (one, two) = Solver(from_str=example_input).solve()
        self.assertEqual(one, 57)
        self.assertEqual(two, 29)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december17.input").solve()
        self.assertEqual(one, 38451)
        self.assertEqual(two, 28142)


if __name__ == "__main__":
    unittest.main()
