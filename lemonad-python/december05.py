"""
December 05, Advent of Code 2018 (Jonas Nockert / @lemonad)

This is an obviously dumb solution (but also quite short and straightforward :)

Edit: Aha, the way to match Aa and aA but not AA with regex is to search
for two consecutive characters that are different but same when compared
without regard to case.

"""
import re

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)
        # Generate regex for "(aA|...|zZ|Aa|...|Zz)".
        # pairs = (
        #     "{0}{1}|{1}{0}".format(chr(c), chr(c).upper())
        #     for c in range(ord("a"), ord("z") + 1)
        # )
        # self.rereact = re.compile("|".join(pairs))
        self.rereact = re.compile(r"(.)(?!\1)(?i:\1)")

    def react(self, polymer):
        n = 1
        while n > 0:
            polymer, n = re.subn(self.rereact, "", polymer)
        return polymer

    def solve_part_one(self):
        """Solution for part one."""
        polymer = self.react(self.puzzle_input)
        return len(polymer)

    def solve_part_two(self):
        """Solution for part two."""
        min_len = len(self.puzzle_input)
        for c in range(ord("a"), ord("z") + 1):
            s, ns = re.subn(chr(c), "", self.puzzle_input, flags=re.I)
            min_len = min(min_len, len(self.react(s)))
        return min_len

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december05.input")
    (one, two) = s.solve()
    print("Units remaining after full reaction:", one)
    print("Shortest polymer:", two)
