"""
Unit tests for December 05, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december05 import Solver


class TestDecember05(unittest.TestCase):
    def test_react(self):
        s = Solver(from_str="...")
        self.assertEqual(s.react("AAa"), "A")
        self.assertEqual(s.react("bAAa"), "bA")
        self.assertEqual(s.react("AA"), "AA")
        self.assertEqual(s.react("aa"), "aa")
        self.assertEqual(s.react("aA"), "")
        self.assertEqual(s.react("Aa"), "")
        self.assertEqual(s.react("abBA"), "")
        self.assertEqual(s.react("aABb"), "")
        self.assertEqual(s.react("CaABbC"), "CC")
        self.assertEqual(s.react("CCaABbC"), "CCC")
        self.assertEqual(s.react("abAB"), "abAB")
        self.assertEqual(s.react("aabAAB"), "aabAAB")

    def test_example_one(self):
        s = Solver(from_str="dabAcCaCBAcCcaDA")
        self.assertEqual(s.react(s.puzzle_input), "dabCBAcaDA")
        (one, two) = s.solve()
        self.assertEqual(one, 10)
        self.assertEqual(two, 4)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december05.input").solve()
        self.assertEqual(one, 10132)
        self.assertEqual(two, 4572)


if __name__ == "__main__":
    unittest.main()
