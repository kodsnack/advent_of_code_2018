"""
December 21, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def halting_gen():
        """Alternative to breakpoint at instruction 28 and checking reg2."""
        reg2 = 0
        while True:
            reg5 = reg2 | 65536
            reg2 = 5234604
            while True:
                reg3 = reg5 & 255
                reg2 = (((reg2 + reg3) & 0xFFFFFF) * 65899) & 0xFFFFFF
                delta = int(reg5 / 256)
                if delta < 1:
                    yield reg2
                    break
                reg3 = delta
                reg5 = reg3

    def solve_part_one(self):
        """Solution for part one."""
        hg = self.halting_gen()
        return hg.__next__()

    def solve_part_two(self):
        """Solution for part two."""
        hg = self.halting_gen()
        seen = set()
        last_seen = None
        while True:
            n = hg.__next__()
            if n in seen:
                return last_seen
            last_seen = n
            seen.add(n)

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december21.input")
    one = s.solve_part_one()
    print("First reg0 value halting after fewest instructions:", one)
    assert one == 13522479

    two = s.solve_part_two()
    print("Reg0 value halting after most instructions:", two)
    assert two == 14626276
