"""
Unit tests for December 02, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december02 import Solver


class TestDecember02(unittest.TestCase):
    def test_example_part_one(self):
        input_str = """
            abcdef
            bababc
            abbcde
            abcccd
            aabcdd
            abcdee
            ababab
            """
        one = Solver(from_str=input_str).solve_part_one()
        self.assertEqual(one, 12)

    def test_example_part_two(self):
        input_str = """
            abcde
            fghij
            klmno
            pqrst
            fguij
            axcye
            wvxyz
            """
        two = Solver(from_str=input_str).solve_part_two()
        self.assertEqual(two, "fgij")

    def test_solution(self):
        (one, two) = Solver(from_file="input/december02.input").solve()
        self.assertEqual(one, 7533)
        self.assertEqual(two, "mphcuasvrnjzzkbgdtqeoylva")


if __name__ == "__main__":
    unittest.main()
