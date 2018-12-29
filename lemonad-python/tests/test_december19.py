"""
Unit tests for December 19, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december19 import Solver


class TestDecember19(unittest.TestCase):
    def test_example(self):
        example_input = """
        #ip 0
        seti 5 0 1
        seti 6 0 2
        addi 0 1 0
        addr 1 2 3
        setr 1 0 0
        seti 8 0 4
        seti 9 0 5
        """
        s = Solver(from_str=example_input)
        one = s.solve_part_one()
        self.assertEqual(one, 7)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december19.input").solve()
        self.assertEqual(one, 2160)
        self.assertEqual(two, 25945920)


if __name__ == "__main__":
    unittest.main()
