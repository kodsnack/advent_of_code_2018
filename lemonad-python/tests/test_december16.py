"""
Unit tests for December 16, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december16 import Solver


class TestDecember16(unittest.TestCase):
    def test_example(self):
        example_input = """
            Before: [3, 2, 1, 1]
            9 2 1 2
            After:  [3, 2, 2, 1]
        """
        s = Solver(from_str=example_input)
        self.assertEqual(s.solve_part_one(), 1)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december16.input").solve()
        self.assertEqual(one, 590)
        self.assertEqual(two, 475)


if __name__ == "__main__":
    unittest.main()
