"""
December 18, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import numpy as np

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)
        np.set_printoptions(threshold=np.nan)
        self.formatter = lambda x: "%s" % x

    def evolve(self, t_lim):
        state = self.as_char_numpy_array()
        width, height = np.shape(state)

        state = np.pad(
            state, pad_width=1, mode="constant", constant_values=(b".", b".")
        )
        mask = np.array([[True, True, True], [True, False, True], [True, True, True]])
        prev = {}

        t = 1
        found = False
        while t <= t_lim:
            next_state = state.copy()
            for x in range(1, width + 1):
                for y in range(1, height + 1):
                    n = state[y - 1 : y + 2, x - 1 : x + 2]
                    n = n[mask]
                    n_trees = np.sum(n == b"|")
                    n_open = np.sum(n == b".")
                    n_lumberyards = np.sum(n == b"#")
                    if state[y, x] == b".":
                        if n_trees >= 3:
                            next_state[y, x] = b"|"
                    elif state[y, x] == b"|":
                        if n_lumberyards >= 3:
                            next_state[y, x] = b"#"
                    elif state[y, x] == b"#":
                        if n_lumberyards >= 1 and n_trees >= 1:
                            pass
                        else:
                            next_state[y, x] = b"."
            if np.all(state == next_state):
                break
            state = next_state
            h = hash(state.data.tobytes())
            if not found and h in prev:
                prev_t = prev[h]
                loop = t - prev_t
                delta = int((t_lim - t) / loop)
                t = t + delta * loop
                found = True
            prev[h] = t
            t += 1

        # print(np.array2string(np.char.decode(state),
        #     separator='', prefix='', suffix='',
        #     max_line_width=np.nan, formatter={'str_kind': formatter}))
        n_trees = np.sum(state == b"|")
        n_lumberyards = np.sum(state == b"#")
        return n_trees * n_lumberyards

    def solve_part_one(self):
        """Solution for part one."""
        return self.evolve(10)

    def solve_part_two(self):
        """Solution for part two."""
        return self.evolve(1000000000)

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december18.input")
    (one, two) = s.solve()
    print("Total resource value after 10 minutes:", one)
    print("Total resource value after 1000000000 minutes:", two)
    assert one == 355918
    assert two == 202806
