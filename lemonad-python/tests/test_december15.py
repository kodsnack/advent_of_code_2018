"""
Unit tests for December 15, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december15 import Solver


class TestDecember15(unittest.TestCase):
    def test_larger_example_of_movement(self):
        example_input = """
            #########
            #G..G..G#
            #.......#
            #.......#
            #G..E..G#
            #.......#
            #.......#
            #G..G..G#
            #########
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one(last_round=1)
        round_str = """
            #########
            #.G...G.#
            #...G...#
            #...E..G#
            #.G.....#
            #.......#
            #G..G..G#
            #.......#
            #########
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)

        one, units, grid_str = s.solve_part_one(last_round=2)
        round_str = """
            #########
            #..G.G..#
            #...G...#
            #.G.E.G.#
            #.......#
            #G..G..G#
            #.......#
            #.......#
            #########
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)

        one, units, grid_str = s.solve_part_one(last_round=3)
        round_str = """
            #########
            #.......#
            #..GGG..#
            #..GEG..#
            #G..G...#
            #......G#
            #.......#
            #.......#
            #########
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)

    def test_example_first_step(self):
        example_input = """
            #######
            #.E...#
            #.....#
            #...G.#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one(last_round=1)
        round_str = """
            #######
            #..E..#
            #...G.#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        one, units, grid_str = s.solve_part_one(last_round=2)
        round_str = """
            #######
            #...E.#
            #...G.#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)

        example_input = """
            #######
            #.E..E#
            #.....#
            #...G.#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one(last_round=1)
        round_str = """
            #######
            #..EE.#
            #...G.#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        one, units, grid_str = s.solve_part_one(last_round=2)
        round_str = """
            #######
            #...E.#
            #..EG.#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)

    def test_example_manhattan_horizontal(self):
        example_input = """
            #######
            #E...G#
            #.....#
            #.....#
            #.....#
            #G...E#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one(last_round=1)
        round_str = """
            #######
            #.E.G.#
            #.....#
            #.....#
            #.....#
            #.G.E.#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        one, units, grid_str = s.solve_part_one(last_round=2)
        round_str = """
            #######
            #..EG.#
            #.....#
            #.....#
            #.....#
            #..GE.#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)

    def test_example_manhattan_vertical(self):
        example_input = """
            #######
            #E...G#
            #.....#
            #.....#
            #G...E#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one(last_round=1)
        round_str = """
            #######
            #.....#
            #E...G#
            #G...E#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)

    def test_example_manhattan_diagonal(self):
        example_input = """
            #######
            #E....#
            #.....#
            #.....#
            #.....#
            #....G#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one(last_round=1)
        round_str = """
            #######
            #.E...#
            #.....#
            #.....#
            #....G#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        one, units, grid_str = s.solve_part_one(last_round=2)
        round_str = """
            #######
            #..E..#
            #.....#
            #....G#
            #.....#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        one, units, grid_str = s.solve_part_one(last_round=3)
        round_str = """
            #######
            #...E.#
            #....G#
            #.....#
            #.....#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        one, units, grid_str = s.solve_part_one(last_round=4)
        round_str = """
            #######
            #....E#
            #....G#
            #.....#
            #.....#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)

        example_input = """
            #######
            #....G#
            #.....#
            #.....#
            #.....#
            #E....#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one(last_round=1)
        round_str = """
            #######
            #...G.#
            #.....#
            #.....#
            #E....#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        one, units, grid_str = s.solve_part_one(last_round=2)
        round_str = """
            #######
            #..G..#
            #.....#
            #E....#
            #.....#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        one, units, grid_str = s.solve_part_one(last_round=3)
        round_str = """
            #######
            #.G...#
            #E....#
            #.....#
            #.....#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        one, units, grid_str = s.solve_part_one(last_round=4)
        round_str = """
            #######
            #G....#
            #E....#
            #.....#
            #.....#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)

    def test_sample_combat(self):
        example_input = """
            #######
            #.G...#
            #...EG#
            #.#.#G#
            #..G#E#
            #.....#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one(last_round=1)
        round_str = """
            #######
            #..G..#
            #...EG#
            #.#G#G#
            #...#E#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((3, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((4, 2), units)["HP"], 197)
        self.assertEqual(Solver.unit_by_loc((5, 2), units)["HP"], 197)
        self.assertEqual(Solver.unit_by_loc((3, 3), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 197)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 197)

        one, units, grid_str = s.solve_part_one(last_round=2)
        round_str = """
            #######
            #...G.#
            #..GEG#
            #.#.#G#
            #...#E#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((4, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((3, 2), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((4, 2), units)["HP"], 188)
        self.assertEqual(Solver.unit_by_loc((5, 2), units)["HP"], 194)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 194)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 194)

        one, units, grid_str = s.solve_part_one(last_round=23)
        round_str = """
            #######
            #...G.#
            #..G.G#
            #.#.#G#
            #...#E#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((4, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((3, 2), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((5, 2), units)["HP"], 131)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 131)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 131)

        one, units, grid_str = s.solve_part_one(last_round=24)
        round_str = """
            #######
            #..G..#
            #...G.#
            #.#G#G#
            #...#E#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((3, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((4, 2), units)["HP"], 131)
        self.assertEqual(Solver.unit_by_loc((3, 3), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 128)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 128)

        one, units, grid_str = s.solve_part_one(last_round=25)
        round_str = """
            #######
            #.G...#
            #..G..#
            #.#.#G#
            #..G#E#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((2, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((3, 2), units)["HP"], 131)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 125)
        self.assertEqual(Solver.unit_by_loc((3, 4), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 125)

        one, units, grid_str = s.solve_part_one(last_round=26)
        round_str = """
            #######
            #G....#
            #.G...#
            #.#.#G#
            #...#E#
            #..G..#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((1, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((2, 2), units)["HP"], 131)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 122)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 122)
        self.assertEqual(Solver.unit_by_loc((3, 5), units)["HP"], 200)

        one, units, grid_str = s.solve_part_one(last_round=27)
        round_str = """
            #######
            #G....#
            #.G...#
            #.#.#G#
            #...#E#
            #...G.#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((1, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((2, 2), units)["HP"], 131)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 119)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 119)
        self.assertEqual(Solver.unit_by_loc((4, 5), units)["HP"], 200)

        one, units, grid_str = s.solve_part_one(last_round=28)
        round_str = """
            #######
            #G....#
            #.G...#
            #.#.#G#
            #...#E#
            #....G#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((1, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((2, 2), units)["HP"], 131)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 116)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 113)
        self.assertEqual(Solver.unit_by_loc((5, 5), units)["HP"], 200)

    def test_example_one(self):
        example_input = """
            #######
            #.G...#
            #...EG#
            #.#.#G#
            #..G#E#
            #.....#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one()
        self.assertEqual(one, 27730)
        round_str = """
            #######
            #G....#
            #.G...#
            #.#.#G#
            #...#.#
            #....G#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((1, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((2, 2), units)["HP"], 131)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 59)
        self.assertEqual(Solver.unit_by_loc((5, 5), units)["HP"], 200)

        two, units, grid_str = s.solve_part_two()
        round_str = """
            #######
            #..E..#
            #...E.#
            #.#.#.#
            #...#.#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((3, 1), units)["HP"], 158)
        self.assertEqual(Solver.unit_by_loc((4, 2), units)["HP"], 14)
        self.assertEqual(two, 4988)

    def test_example_two(self):
        example_input = """
            #######
            #G..#E#
            #E#E.E#
            #G.##.#
            #...#E#
            #...E.#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one()
        round_str = """
            #######
            #...#E#
            #E#...#
            #.E##.#
            #E..#E#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((5, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((1, 2), units)["HP"], 197)
        self.assertEqual(Solver.unit_by_loc((2, 3), units)["HP"], 185)
        self.assertEqual(Solver.unit_by_loc((1, 4), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 200)
        self.assertEqual(one, 36334)

    def test_example_three(self):
        example_input = """
            #######
            #E..EG#
            #.#G.E#
            #E.##E#
            #G..#.#
            #..E#.#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one()
        self.assertEqual(one, 39514)
        round_str = """
            #######
            #.E.E.#
            #.#E..#
            #E.##.#
            #.E.#.#
            #...#.#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((2, 1), units)["HP"], 164)
        self.assertEqual(Solver.unit_by_loc((4, 1), units)["HP"], 197)
        self.assertEqual(Solver.unit_by_loc((3, 2), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((1, 3), units)["HP"], 98)
        self.assertEqual(Solver.unit_by_loc((2, 4), units)["HP"], 200)

        two, units, grid_str = s.solve_part_two()
        self.assertEqual(two, 31284)
        round_str = """
            #######
            #.E.E.#
            #.#E..#
            #E.##E#
            #.E.#.#
            #...#.#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((2, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((4, 1), units)["HP"], 23)
        self.assertEqual(Solver.unit_by_loc((3, 2), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((1, 3), units)["HP"], 125)
        self.assertEqual(Solver.unit_by_loc((5, 3), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((2, 4), units)["HP"], 200)

    def test_example_four(self):
        example_input = """
            #######
            #E.G#.#
            #.#G..#
            #G.#.G#
            #G..#.#
            #...E.#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one()
        self.assertEqual(one, 27755)
        round_str = """
            #######
            #G.G#.#
            #.#G..#
            #..#..#
            #...#G#
            #...G.#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((1, 1), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((3, 1), units)["HP"], 98)
        self.assertEqual(Solver.unit_by_loc((3, 2), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((5, 4), units)["HP"], 95)
        self.assertEqual(Solver.unit_by_loc((4, 5), units)["HP"], 200)

        two, units, grid_str = s.solve_part_two()
        self.assertEqual(two, 3478)
        round_str = """
            #######
            #.E.#.#
            #.#E..#
            #..#..#
            #...#.#
            #.....#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((2, 1), units)["HP"], 8)
        self.assertEqual(Solver.unit_by_loc((3, 2), units)["HP"], 86)

    def test_example_five(self):
        example_input = """
            #######
            #.E...#
            #.#..G#
            #.###.#
            #E#G#G#
            #...#G#
            #######
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one()
        self.assertEqual(one, 28944)
        round_str = """
            #######
            #.....#
            #.#G..#
            #.###.#
            #.#.#.#
            #G.G#G#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((3, 2), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((1, 5), units)["HP"], 98)
        self.assertEqual(Solver.unit_by_loc((3, 5), units)["HP"], 38)
        self.assertEqual(Solver.unit_by_loc((5, 5), units)["HP"], 200)

        two, units, grid_str = s.solve_part_two()
        self.assertEqual(two, 6474)
        round_str = """
            #######
            #...E.#
            #.#..E#
            #.###.#
            #.#.#.#
            #...#.#
            #######
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((4, 1), units)["HP"], 14)
        self.assertEqual(Solver.unit_by_loc((5, 2), units)["HP"], 152)

    def test_example_six(self):
        example_input = """
            #########
            #G......#
            #.E.#...#
            #..##..G#
            #...##..#
            #...#...#
            #.G...G.#
            #.....G.#
            #########
        """
        s = Solver(from_str=example_input)
        one, units, grid_str = s.solve_part_one()
        self.assertEqual(one, 18740)
        round_str = """
            #########
            #.G.....#
            #G.G#...#
            #.G##...#
            #...##..#
            #.G.#...#
            #.......#
            #.......#
            #########
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(Solver.unit_by_loc((2, 1), units)["HP"], 137)
        self.assertEqual(Solver.unit_by_loc((1, 2), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((3, 2), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((2, 3), units)["HP"], 200)
        self.assertEqual(Solver.unit_by_loc((2, 5), units)["HP"], 200)
        self.assertEqual(grid_str, round_str)

        two, units, grid_str = s.solve_part_two()
        self.assertEqual(two, 1140)
        round_str = """
            #########
            #.......#
            #.E.#...#
            #..##...#
            #...##..#
            #...#...#
            #.......#
            #.......#
            #########
        """.strip().replace(
            " ", ""
        )
        self.assertEqual(grid_str, round_str)
        self.assertEqual(Solver.unit_by_loc((2, 2), units)["HP"], 38)

    def test_solution(self):
        s = Solver(from_file="input/december15.input")
        one, units, grid_str = s.solve_part_one()
        self.assertEqual(one, 246176)
        two, units, grid_str = s.solve_part_two()
        self.assertEqual(two, 58128)


if __name__ == "__main__":
    unittest.main()
