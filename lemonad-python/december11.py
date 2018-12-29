"""
December 11, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import numpy as np
from scipy.signal import correlate

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    def powergrid(self, gsn):
        Y, X = np.mgrid[1:301, 1:301]
        return (((X + 10) * Y + gsn) * (X + 10)) // 100 % 10 - 5

    def solve_part_one(self):
        """Solution for part one."""
        gsn = self.as_int()
        p = self.powergrid(gsn)

        s = correlate(p, np.ones([3, 3], dtype=int), "valid")
        iy, ix = np.unravel_index(np.argmax(s, axis=None), s.shape)
        return ("{:d},{:d}".format(ix + 1, iy + 1), s[iy, ix])

    def solve_part_two(self):
        """Solution for part two."""
        gsn = self.as_int()
        p = self.powergrid(gsn)

        smax = 0
        ixmax = None
        szmax = 0
        for sz in range(1, 300):
            s = correlate(p, np.ones([sz, sz], dtype=int), "valid")
            ix = np.unravel_index(np.argmax(s, axis=None), s.shape)
            if s[ix] > smax:
                smax = s[ix]
                ixmax = ix
                szmax = sz
        return ("{:d},{:d},{:d}".format(ixmax[1] + 1, ixmax[0] + 1, szmax), smax)

    def solve(self):
        return (self.solve_part_one()[0], self.solve_part_two()[0])


if __name__ == "__main__":
    s = Solver(from_file="input/december11.input")
    (one, two) = s.solve()
    print("X,Y of top-left 3x3 square", one)
    print("X,Y,size of top-left NxN square", two)
