"""
December 25, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from itertools import combinations

import numpy as np
from scipy.spatial.distance import pdist

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def find_root(r, refs):
        while r != refs[r]:
            r = refs[r]
        return r

    def solve_part_one(self):
        """Solution for part one."""
        lines = list(self.lines_split(",", conversion=int))
        n_lines = len(lines)

        refs = {}
        m = np.zeros((n_lines, 4), dtype="i")
        for i, nums in enumerate(lines):
            m[i, :] = np.array(nums, dtype="i")
            refs[i] = i

        dists = pdist(m, metric="cityblock")
        for i, p in enumerate(combinations(range(n_lines), 2)):
            p0, p1 = p
            if dists[i] <= 3:
                if refs[p1] == p1:
                    refs[p1] = p0
                else:
                    r0 = self.find_root(p0, refs)
                    r1 = self.find_root(p1, refs)
                    if r0 != r1:
                        refs[r1] = r0

        # Flatten chained unions.
        for r in refs:
            ri = self.find_root(r, refs)
            refs[r] = ri

        return len(set(refs.values()))

    def solve(self):
        pass


if __name__ == "__main__":
    s = Solver(from_file="input/december25.input")
    one = s.solve_part_one()
    print("Constellations formed by the fixed points in spacetime:", one)
