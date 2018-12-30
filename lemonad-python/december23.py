"""
December 23, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from itertools import product
import re

from pulp import (
    LpMaximize,
    LpMinimize,
    LpProblem,
    LpStatus,
    LpStatusOptimal,
    lpSum,
    LpVariable,
    value,
)
import numpy as np

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    def solve_part_one(self):
        """Solution for part one."""
        lines = list(self.lines())
        pos = np.zeros((len(lines), 4), dtype="int64")
        for i, line in enumerate(lines):
            m = re.search("pos=<(-*\d+),(-*\d+),(-*\d+)>, r=(\d+)", line)
            pos[i, :] = np.array(
                [int(m.group(1)), int(m.group(2)), int(m.group(3)), int(m.group(4))],
                dtype="int64",
            )
        max_r = np.max(pos[:, 3])

        max_in_range = 0
        for m in range(len(pos)):
            if pos[m, 3] != max_r:
                continue

            in_range = 0
            for n in range(len(pos)):
                mdist = np.linalg.norm(pos[m, 0:3] - pos[n, 0:3], 1)
                if mdist <= pos[m, 3]:
                    in_range += 1
            max_in_range = max(in_range, max_in_range)

        return max_in_range

    def solve_part_two(self):
        """Solution for part two."""
        lines = list(self.lines())
        pos = np.zeros((len(lines), 4), dtype="int64")
        for i, line in enumerate(lines):
            m = re.search("pos=<(-*\d+),(-*\d+),(-*\d+)>, r=(\d+)", line)
            pos[i, :] = np.array(
                [int(m.group(1)), int(m.group(2)), int(m.group(3)), int(m.group(4))],
                dtype="int64",
            )
        max_r = np.max(pos[:, 3])

        prob = LpProblem("bots", LpMinimize)
        # Bots active low to simplify code.
        bots = [LpVariable("bot{:d}".format(i), cat="Binary") for i in range(len(pos))]
        n_bots = LpVariable("Number of bots", cat="Integer")
        x = LpVariable("x", cat="Integer")
        y = LpVariable("y", cat="Integer")
        z = LpVariable("z", cat="Integer")

        prob += n_bots
        prob += n_bots == sum(bots)
        for i, (xi, yi, zi, ri) in enumerate(pos):
            # Cf.
            # https://stackoverflow.com/questions/29795632/sum-of-absolute-values-constraint-in-semi-definite-programming
            for p in product([-1, 1], repeat=3):
                prob += (
                    (p[0] * (x - xi) + p[1] * (y - yi) + p[2] * (z - zi))
                    <= ri + bots[i] * max_r * 2
                )  # Not sure why max_r was not enough.
        status = prob.solve()
        assert status == LpStatusOptimal
        # print("(x, y, z) =", (value(x), value(y), value(z)))
        # print("number of active nanobots", len(bots) - value(n_bots))
        # For some reason, this finds the solution right away without
        # local search.
        return int(np.linalg.norm((value(x), value(y), value(z)), 1))

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december23.input")
    (one, two) = s.solve()
    print("Nanobots are in range: {:d}".format(one))
    print("Coordinates in range of largest number of nanobots: {:d}".format(two))
