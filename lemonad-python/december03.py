"""
December 03, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import numpy as np

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    def get_sizes(self):
        """Get size of fabric given input."""
        max_width = 0
        max_height = 0
        for m in self.lines_search("\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"):
            left = int(m.group(2))
            top = int(m.group(3))
            width = int(m.group(4))
            height = int(m.group(5))
            max_width = max(max_width, left + width)
            max_height = max(max_height, top + height)
        return (max_height, max_width)

    def solve_part_one(self):
        """Solution for part one."""
        fabric = np.zeros(self.get_sizes())
        for m in self.lines_search("\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"):
            left = int(m.group(2))
            top = int(m.group(3))
            width = int(m.group(4))
            height = int(m.group(5))
            fabric[top : top + height, left : left + width] += 1
        return np.sum(np.sum(fabric >= 2))

    def solve_part_two(self):
        """Solution for part two."""
        fabric = np.zeros(self.get_sizes())
        # Update fabric.
        for m in self.lines_search("\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"):
            left = int(m.group(2))
            top = int(m.group(3))
            width = int(m.group(4))
            height = int(m.group(5))
            fabric[top : top + height, left : left + width] += 1

        # If an area has all 1's it does not overlap with any other area.
        for m in self.lines_search("\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"):
            fid = int(m.group(1))
            left = int(m.group(2))
            top = int(m.group(3))
            width = int(m.group(4))
            height = int(m.group(5))
            if np.all(fabric[top : top + height, left : left + width] == 1):
                return fid
        return None

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december03.input")
    (one, two) = s.solve()
    print("Square inches of fabric within two or more claims: ", one)
    print("ID of the only claim that doesn't overlap: ", two)
