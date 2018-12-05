"""
December 02, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from collections import Counter

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    def solve_part_one(self):
        """Solution for part one."""
        twos = 0
        threes = 0
        for line in self.lines():
            # 'abdefabc' -> dict_values([2, 2, 2, 1, 1, 1])
            vals = Counter(line).values()
            if 2 in vals:
                twos += 1
            if 3 in vals:
                threes += 1
        return twos * threes

    def solve_part_two(self):
        """Solution for part two."""
        box_ids_with_missing_char = {}
        for line in self.lines():
            # A line with N characters gets stored N times in the hash.
            # "abcd" -> "bcd" [0], "acd" [1], "abd" [2], "abc" [3].
            for i in range(len(line)):
                # Remove one character and save the index in hash, this
                # way we can separate a[b]cd acd[e], where [x] is the
                # removed character.
                xline = line[:i] + line[(i + 1) :]
                if (
                    xline in box_ids_with_missing_char
                    and box_ids_with_missing_char[xline] == i
                ):
                    return xline
                box_ids_with_missing_char[xline] = i
        return None

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december02.input")
    (one, two) = s.solve()
    print("Checksum for your list of box IDs: {:d}".format(one))
    print("Common letters: {:s}".format(two))
