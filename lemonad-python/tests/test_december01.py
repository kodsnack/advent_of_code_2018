import unittest

from december01 import Solver


class TestDecember01(unittest.TestCase):
    def test_example_part1(self):
        example_input = """
        +1
        -2
        +3
        +1
        """
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, 3)

    def test_other_examples_part1(self):
        example_input = """
        +1
        +1
        +1
        """
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, 3)

        example_input = """
        +1
        +1
        -2
        """
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, 0)

        example_input = """
        -1
        -2
        -3
        """
        one = Solver(from_str=example_input).solve_part_one()
        self.assertEqual(one, -6)

    def test_example_part2(self):
        example_input = """
        +1
        -2
        +3
        +1
        """
        two = Solver(from_str=example_input).solve_part_two()

        self.assertEqual(two, 2)

    def test_other_examples_part2(self):
        example_input = """
        +1
        -1
        """
        two = Solver(from_str=example_input).solve_part_two()
        self.assertEqual(two, 0)

        example_input = """
        +3
        +3
        +4
        -2
        -4
        """
        two = Solver(from_str=example_input).solve_part_two()
        self.assertEqual(two, 10)

        example_input = """
        -6
        +3
        +8
        +5
        -6
        """
        two = Solver(from_str=example_input).solve_part_two()
        self.assertEqual(two, 5)

        example_input = """
        +7
        +7
        -2
        -7
        -4
        """
        two = Solver(from_str=example_input).solve_part_two()
        self.assertEqual(two, 14)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december01.input").solve()
        self.assertEqual(one, 425)
        self.assertEqual(two, 57538)


if __name__ == "__main__":
    unittest.main()
