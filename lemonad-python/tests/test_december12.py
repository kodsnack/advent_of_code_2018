"""
Unit tests for December 12, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december12 import Solver


class TestDecember12(unittest.TestCase):
    def test_example_one(self):
        example_input = """
            initial state: #..#.#..##......###...###

            ...## => #
            ..#.. => #
            .#... => #
            .#.#. => #
            .#.## => #
            .##.. => #
            .#### => #
            #.#.# => #
            #.### => #
            ##.#. => #
            ##.## => #
            ###.. => #
            ###.# => #
            ####. => #
            """
        s = Solver(from_str=example_input)
        one = s.solve_part_one()
        self.assertEqual(one, 325)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december12.input").solve()
        self.assertEqual(one, 3738)
        self.assertEqual(two, 3900000002467)


if __name__ == "__main__":
    unittest.main()
