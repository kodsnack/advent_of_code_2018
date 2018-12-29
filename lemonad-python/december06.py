"""
December 06, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import numpy as np
from scipy import spatial

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    def get_points(self):
        """Get points from input."""
        P = np.zeros((0, 2), dtype="int")
        for x, y in self.lines_split(",", conversion=int):
            P = np.append(P, [[x, y]], 0)

        m = np.mean(P, axis=0)
        center = np.array([int(m[0]), int(m[1])])
        return P, center

    @staticmethod
    def perimeter_counts(P, center, radius, below_limit=None):
        """Get region counts along a rectangular perimeter."""
        min_x = center[0] - radius
        max_x = center[0] + radius
        min_y = center[1] - radius
        max_y = center[1] + radius
        if radius == 0:
            n_el = 1
        else:
            n_el = 2 * max(0, max_x - min_x + 1) + 2 * max(0, max_y - min_y - 1)

        R = np.zeros((n_el, 2), dtype="int")
        i = 0
        # Include corners for x but not for y.
        for x in range(min_x, max_x + 1):
            R[i] = [x, min_y]
            i += 1
            if radius > 0:
                R[i] = [x, max_y]
                i += 1
        for y in range(min_y + 1, max_y):
            if radius > 0:
                R[i] = [min_x, y]
                R[i + 1] = [max_x, y]
                i += 2

        n_below = 0
        counts = np.zeros((len(P) + 1, 1))
        for r in R:
            Y = spatial.distance.cdist(P, [r], metric="cityblock")
            ix = np.argpartition(Y, 0, axis=None)[0]
            ix2 = np.argpartition(Y, 1, axis=None)[1]
            if Y[ix] == Y[ix2]:
                counts[0] += 1
            else:
                counts[ix + 1] += 1
            if below_limit and np.sum(Y) < below_limit:
                n_below += 1
        return counts, n_below

    def solve_part_one(self):
        """Solution for part one."""
        P, center = self.get_points()
        # Assume all regions present on distant perimeter has
        # infinite area.
        infcounts, _ = self.perimeter_counts(P, center, 500)
        infcounts_nz = infcounts[1:] != 0

        areas = np.zeros((len(P), 1))
        r = 0
        while True:
            counts, _ = self.perimeter_counts(P, center, r)
            counts = counts[1:]
            if np.sum(counts[~infcounts_nz]) == 0:
                break
            areas += counts
            r += 1
        return int(np.max(areas[~infcounts_nz]))

    def solve_part_two(self, limit=10000):
        """Solution for part two."""
        P, center = self.get_points()

        n_total_below = 0
        r = 0
        while True:
            counts, n_below = self.perimeter_counts(P, center, r, below_limit=limit)
            if n_total_below > 0 and not n_below:
                break
            n_total_below += n_below
            r += 1
        return n_total_below

    def solve(self, limit=10000):
        return (self.solve_part_one(), self.solve_part_two(limit=limit))


if __name__ == "__main__":
    s = Solver(from_file="input/december06.input")
    (one, two) = s.solve()
    print("Size of the largest area that isn't infinite:", one)
    print(
        "size of the region containing all locations which have a total distance "
        "to all given coordinates of less than 10000:",
        two,
    )
