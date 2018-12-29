"""
Unit tests for December 18, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december18 import Solver


class TestDecember18(unittest.TestCase):
    def test_sum_of_natural_numbers(self):
        example_input = """
.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.
        """
        s = Solver(from_str=example_input)
        self.assertEqual(s.solve_part_one(), 1147)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december18.input").solve()
        self.assertEqual(one, 355918)
        self.assertEqual(two, 202806)


if __name__ == "__main__":
    unittest.main()
