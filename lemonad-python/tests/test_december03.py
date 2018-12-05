"""
Unit tests for December 03, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december03 import Solver


class TestDecember03(unittest.TestCase):
    example_input = """
        #1 @ 1,3: 4x4
        #2 @ 3,1: 4x4
        #3 @ 5,5: 2x2
        """

    def test_get_matrix_size(self):
        s = Solver(from_str="#123 @ 3,2: 5x4")
        (h, w) = s.get_sizes()
        self.assertEqual(h, 6)
        self.assertEqual(w, 8)

        s = Solver(from_str=self.example_input)
        (h, w) = s.get_sizes()
        self.assertEqual(h, 7)
        self.assertEqual(w, 7)

    def test_example_part_one(self):
        one = Solver(from_str=self.example_input).solve_part_one()
        self.assertEqual(one, 4)

    def test_example_part_two(self):
        two = Solver(from_str=self.example_input).solve_part_two()
        self.assertEqual(two, 3)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december03.input").solve()
        self.assertEqual(one, 111935)
        self.assertEqual(two, 650)


if __name__ == "__main__":
    unittest.main()
