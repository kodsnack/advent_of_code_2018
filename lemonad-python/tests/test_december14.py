"""
Unit tests for December 14, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december14 import Solver


class TestDecember14(unittest.TestCase):
    def test_examples_one(self):
        s = Solver(from_str="9")
        self.assertEqual(s.solve_part_one(), "5158916779")
        s = Solver(from_str="5")
        self.assertEqual(s.solve_part_one(), "0124515891")
        s = Solver(from_str="18")
        self.assertEqual(s.solve_part_one(), "9251071085")
        s = Solver(from_str="2018")
        self.assertEqual(s.solve_part_one(), "5941429882")

    def test_examples_two(self):
        s = Solver(from_str="51589")
        self.assertEqual(s.solve_part_two(), 9)
        s = Solver(from_str="01245")
        self.assertEqual(s.solve_part_two(), 5)
        s = Solver(from_str="92510")
        self.assertEqual(s.solve_part_two(), 18)
        s = Solver(from_str="59414")
        self.assertEqual(s.solve_part_two(), 2018)

    def test_solution(self):
        (one, two) = Solver(from_file='input/december14.input').solve()
        self.assertEqual(one, "3138510102")
        self.assertEqual(two, 20179081)


if __name__ == "__main__":
    unittest.main()
