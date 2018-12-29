"""
Unit tests for December 11, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december11 import Solver


class TestDecember11(unittest.TestCase):
    def test_example1(self):
        s = Solver(from_str="18")
        one, one_total_power = s.solve_part_one()
        two, two_total_power = s.solve_part_two()
        self.assertEqual(one, "33,45")
        self.assertEqual(one_total_power, 29)
        self.assertEqual(two, "90,269,16")
        self.assertEqual(two_total_power, 113)

    def test_example2(self):
        s = Solver(from_str="42")
        one, one_total_power = s.solve_part_one()
        two, two_total_power = s.solve_part_two()
        self.assertEqual(one, "21,61")
        self.assertEqual(one_total_power, 30)
        self.assertEqual(two, "232,251,12")
        self.assertEqual(two_total_power, 119)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december11.input").solve()
        self.assertEqual(one, "21,54")
        self.assertEqual(two, "236,268,11")


if __name__ == "__main__":
    unittest.main()
