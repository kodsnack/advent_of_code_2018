"""
Unit tests for December 22, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december22 import Solver


class TestDecember22(unittest.TestCase):
    def test_example_one(self):
        example_input = """
            depth: 510
            target: 10,10
        """
        s = Solver(from_str=example_input)
        self.assertEqual(s.solve_part_one(), 114)

    def test_example_two(self):
        example_input = """
            depth: 510
            target: 10,10
        """
        s = Solver(from_str=example_input)
        self.assertEqual(s.solve_part_two(), 45)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december22.input").solve()
        self.assertEqual(one, 11843)
        self.assertEqual(two, 1078)


if __name__ == "__main__":
    unittest.main()
