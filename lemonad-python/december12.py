"""
December 12, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import numpy as np

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    formatter = {"bool": lambda x: "#" if x else "."}

    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)
        np.set_printoptions(threshold=np.nan)
        self.read_input_state()

    def read_input_state(self):
        state = None
        trans = []
        res = []

        for line in self.lines():
            if not line:
                continue
            line = line.replace("#", "T")
            line = line.replace(".", "\00")
            if line[0] == "i":
                state = np.frombuffer(line[15:].encode(), dtype="bool")
                state = np.append([False] * 5, state)
                state = np.append(state, [False] * 5)
            else:
                x = np.frombuffer(line[0:5].encode(), dtype="bool")
                y = np.frombuffer(line[9].encode(), dtype="bool")
                trans.append(x)
                res.append(y)
        self.initial_state = state
        self.trans = trans
        self.res = res

    @classmethod
    def state_as_str(cls, state):
        sstr = np.array2string(
            state,
            formatter=cls.formatter,
            separator="",
            prefix="",
            suffix="",
            max_line_width=np.nan,
        )
        return sstr[1:-1]

    def generate(self, n_generations):
        sstart = -5
        cache = {}
        state = self.initial_state.copy()

        sstr = self.state_as_str(state)

        n = 1
        found = False
        while n < n_generations + 1:
            new_state = np.zeros(len(state), dtype="bool")
            for j in range(len(state) - 5):
                for k in range(len(self.trans)):
                    c = state[j : j + 5] == self.trans[k]
                    if c.all():
                        new_state[j + 2] = self.res[k]
            s = min(min(np.argwhere(new_state)))
            e = max(max(np.argwhere(new_state)))
            sstart = sstart + s - 5
            state2 = new_state[s : e + 1]
            state2 = np.append([False] * 5, state2)
            state2 = np.append(state2, [False] * 5)
            sstr = self.state_as_str(state2)

            if not found:
                sstr = hash(new_state.data.tobytes())
                if sstr in cache:
                    sstart += n_generations - n
                    found = True
                    break
                cache[sstr] = n

            state = state2
            n += 1

        s = 0
        for j in range(len(state)):
            if state[j]:
                s += j + sstart
        return s

    def solve_part_one(self):
        """Solution for part one."""
        return self.generate(20)

    def solve_part_two(self):
        """Solution for part two."""
        return self.generate(50000000000)

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december12.input")
    one = s.solve_part_one()
    print("After 20 generations:", one)
    two = s.solve_part_two()
    print("After 50000000000 generations:", two)
