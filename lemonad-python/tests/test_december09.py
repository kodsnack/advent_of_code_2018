"""
Unit tests for December 09, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december09 import Solver


class TestDecember09(unittest.TestCase):
    def test_examples_one(self):
        example_input = "9 players; last marble is worth 25 points"
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, 32)

        example_input = "10 players; last marble is worth 1618 points"
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, 8317)

        example_input = "13 players; last marble is worth 7999 points"
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, 146373)

        example_input = "17 players; last marble is worth 1104 points"
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, 2764)

        example_input = "21 players; last marble is worth 6111 points"
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, 54718)

        example_input = "30 players; last marble is worth 5807 points"
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, 37305)

    def test_solution(self):
        s = Solver(from_file="input/december09.input")
        one = s.solve_part_one()
        self.assertEqual(one, 436720)
        # Second part takes too long time.
        # two = s.solve_part_two()
        # self.assertEqual(two, 3527845091)


if __name__ == "__main__":
    unittest.main()
