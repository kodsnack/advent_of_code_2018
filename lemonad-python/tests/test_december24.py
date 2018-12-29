"""
Unit tests for December 24, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december24 import Solver


class TestDecember24(unittest.TestCase):
    example_input = """
        Immune System:
        17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
        989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

        Infection:
        801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
        4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
    """

    def test_example_one(self):
        s = Solver(from_str=self.example_input)
        self.assertEqual(s.solve_part_one(), 5216)

    def test_example_two(self):
        s = Solver(from_str=self.example_input)
        self.assertEqual(s.solve_part_two(), 51)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december24.input").solve()
        self.assertEqual(one, 18717)
        self.assertEqual(two, 5252)


if __name__ == "__main__":
    unittest.main()
