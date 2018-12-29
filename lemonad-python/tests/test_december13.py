"""
Unit tests for December 13, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december13 import Solver


class TestDecember13(unittest.TestCase):
    def test_example_one(self):
        example_input = """
/->-\\        
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/     
"""
        s = Solver(from_str=example_input, strip=False)
        one = s.solve_part_one()
        self.assertEqual(one, (7, 3))

    def test_example_two(self):
        example_input = """
/>-<\\  
|   |  
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/
"""
        s = Solver(from_str=example_input, strip=False)
        two = s.solve_part_two()
        self.assertEqual(two, (6, 4))

    def test_solution(self):
        (one, two) = Solver(from_file="input/december13.input", strip=False).solve()
        self.assertEqual(one, (64, 57))
        self.assertEqual(two, (136, 8))


if __name__ == "__main__":
    unittest.main()
