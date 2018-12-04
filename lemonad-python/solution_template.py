"""
December DayDD, Advent of Code YearYYYY (Jonas Nockert / @lemonad)

"""
from enum import Enum
import math
import os
import re
import string

import numpy as np
import pandas as pd

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def subproblem(indata):
        pass

    def solve_part_one(self):
        """Solution for part one."""
        return None

    def solve_part_two(self):
        """Solution for part two."""
        return None

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


def main(input_data):
    s = Solver(from_str="...")
    one = s.solve_part_one()
    print(one)
    return

    s = Solver(from_str="...")
    two = s.solve_part_two()
    print(two)
    return

    # assert(s.solve_part_one() == 0)
    # assert(s.solve_part_two() == 0)

    # s = Solver(from_str=input_data)
    s = Solver(from_file="InputFILENAME")
    (one, two) = s.solve()
    print(one)
    print(two)
    # print("{:s}".format(one))
    # print("{:s}".format(two))
    # assert(one == 0)
    # assert(two == 0)


if __name__ == "__main__":
    input_data = """
InputCONTENT
    """.strip()
    main(input_data)
