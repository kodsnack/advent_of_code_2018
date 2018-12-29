"""
Unit tests for December 25, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december25 import Solver


class TestDecember25(unittest.TestCase):
    def test_example1(self):
        example_input = """
            0,0,0,0
            3,0,0,0
            0,3,0,0
            0,0,3,0
            0,0,0,3
            0,0,0,6
            9,0,0,0
            12,0,0,0
        """
        s = Solver(from_str=example_input)
        self.assertEqual(s.solve_part_one(), 2)

    def test_example2(self):
        example_input = """
            -1,2,2,0
            0,0,2,-2
            0,0,0,-2
            -1,2,0,0
            -2,-2,-2,2
            3,0,2,-1
            -1,3,2,2
            -1,0,-1,0
            0,2,1,-2
            3,0,0,0
        """
        s = Solver(from_str=example_input)
        self.assertEqual(s.solve_part_one(), 4)

    def test_example3(self):
        example_input = """
            1,-1,0,1
            2,0,-1,0
            3,2,-1,0
            0,0,3,1
            0,0,-1,-1
            2,3,-2,0
            -2,2,0,0
            2,-2,0,-1
            1,-1,0,-1
            3,2,0,2
        """
        s = Solver(from_str=example_input)
        self.assertEqual(s.solve_part_one(), 3)

    def test_example4(self):
        example_input = """
            1,-1,-1,-2
            -2,-2,0,1
            0,2,1,3
            -2,3,-2,1
            0,2,3,-2
            -1,-1,1,-2
            0,-2,-1,0
            -2,2,3,-1
            1,2,2,0
            -1,-2,0,-2
            """
        s = Solver(from_str=example_input)
        self.assertEqual(s.solve_part_one(), 8)

    def test_solution(self):
        s = Solver(from_file="input/december25.input")
        self.assertEqual(s.solve_part_one(), 370)


if __name__ == "__main__":
    unittest.main()
