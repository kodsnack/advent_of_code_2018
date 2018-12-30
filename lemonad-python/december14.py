"""
December 14, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    def solve_part_one(self):
        """Solution for part one."""
        current = [0, 1]
        scoreboard = [3, 7]
        l_sb = len(scoreboard)
        n = self.as_int()

        while True:
            if l_sb >= n + 10:
                break
            new_scores = [
                int(r) for r in list(str(sum([scoreboard[c] for c in current])))
            ]
            l_new = len(new_scores)
            scoreboard.extend(new_scores)
            l_sb += l_new
            current = [(c + scoreboard[c] + 1) % l_sb for c in current]
        return "".join([str(d) for d in scoreboard[n : n + 10]])

    def solve_part_two(self):
        """Solution for part two."""
        scoreboard = [3, 7]
        l_sb = len(scoreboard)
        current = [0, 1]
        target = [int(d) for d in list(self.puzzle_input)]
        l_t = len(target)

        while True:
            new_scores = [
                int(r) for r in list(str(sum([scoreboard[c] for c in current])))
            ]
            l_new = len(new_scores)
            scoreboard.extend(new_scores)
            l_sb += l_new
            current = [(c + scoreboard[c] + 1) % l_sb for c in current]
            for k in range(l_new):
                if scoreboard[(l_sb - l_t - k) : (l_sb - k)] == target:
                    return l_sb - l_t - k
        return None

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december14.input")
    (one, two) = s.solve()
    print("Scores of ten recipes: {:s}".format(one))
    print("Recipes to the left: {:d}".format(two))
