"""
December 01, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import math
import os
import string

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    def solve_part_one(self):
        """Solution for part one."""
        return sum([int(x) for x in self.lines()])

    def solve_part_two(self):
        """Solution for part two."""
        freq = 0
        freqs = {0: True}
        found = False
        while not found:
            for x in self.lines():
                freq += int(x) 
                if freq in freqs:
                    found = True
                    break
                freqs[freq] = True
        return freq

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december01.input")
    (one, two) = s.solve()
    print("Resulting frequency: {:d}".format(one))
    print("First frequency reached first: {:d}".format(two))
