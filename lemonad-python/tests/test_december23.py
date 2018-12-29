"""
Unit tests for December 23, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december23 import Solver


class TestDecember23(unittest.TestCase):
    def test_example_one(self):
        example_input = """
        pos=<0,0,0>, r=4
        pos=<1,0,0>, r=1
        pos=<4,0,0>, r=3
        pos=<0,2,0>, r=1
        pos=<0,5,0>, r=3
        pos=<0,0,3>, r=1
        pos=<1,1,1>, r=1
        pos=<1,1,2>, r=1
        pos=<1,3,1>, r=1
        """
        s = Solver(from_str=example_input)
        one = s.solve_part_one()
        self.assertEqual(one, 7)

    def test_example_two(self):
        example_input = """
        pos=<10,12,12>, r=2
        pos=<12,14,12>, r=2
        pos=<16,12,12>, r=4
        pos=<14,14,14>, r=6
        pos=<50,50,50>, r=200
        pos=<10,10,10>, r=5
        """
        s = Solver(from_str=example_input)
        two = s.solve_part_two()
        self.assertEqual(two, 36)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december23.input").solve()
        self.assertEqual(one, 408)
        self.assertEqual(two, 121167568)


if __name__ == "__main__":
    unittest.main()
