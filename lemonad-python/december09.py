"""
December 09, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)
        m = self.search(r"(\d+) players; last marble is worth (\d+) point")
        self.n_players = int(m.group(1))
        self.n_points = int(m.group(2))

    def winning_score(self, last_marble_multiplier=1):
        n_points = self.n_points * last_marble_multiplier

        min_marble = 1
        scores = [0] * self.n_players
        circle = [0]
        current_marble_ix = 0
        player = 0
        while min_marble <= n_points:
            # Find which marble to place
            marble_to_place = min_marble
            min_marble += 1

            if (marble_to_place % 23) == 0:
                scores[player - 1] += marble_to_place
                remove_ix = (current_marble_ix - 7) % len(circle)
                scores[player - 1] += circle[remove_ix]

                del circle[remove_ix]
                current_marble_ix = remove_ix
            else:
                ix = (current_marble_ix + 2) % len(circle)
                circle.insert(ix, marble_to_place)
                current_marble_ix = ix
            player = (player + 1) % self.n_players
        return max(scores)

    def solve_part_one(self):
        """Solution for part one."""
        return self.winning_score()

    def solve_part_two(self):
        """Solution for part two."""
        return self.winning_score(last_marble_multiplier=100)

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december09.input")
    one = s.solve_part_one()
    print("Winning Elf's score:", one)
    # Takes too long time to run:
    # two = s.solve_part_two()
    # print("Winning Elf's score if number of last marble is 100 times larger:", two)
