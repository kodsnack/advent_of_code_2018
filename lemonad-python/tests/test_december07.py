"""
Unit tests for December 07, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december07 import Solver


class TestDecember07(unittest.TestCase):
    def test_example(self):
        ex = """
        Step C must be finished before step A can begin.
        Step C must be finished before step F can begin.
        Step A must be finished before step B can begin.
        Step A must be finished before step D can begin.
        Step B must be finished before step E can begin.
        Step D must be finished before step E can begin.
        Step F must be finished before step E can begin.
        """
        s = Solver(from_str=ex)
        one = s.solve_part_one()
        two = s.solve_part_two(n_workers=2, extra_time=0)
        self.assertEqual(one, "CABDFE")
        self.assertEqual(two, 15)

    def test_solution(self):
        s = Solver(from_file="input/december07.input")
        one = s.solve_part_one()
        two = s.solve_part_two(n_workers=5, extra_time=60)
        self.assertEqual(one, "PFKQWJSVUXEMNIHGTYDOZACRLB")
        self.assertEqual(two, 864)


if __name__ == "__main__":
    unittest.main()
