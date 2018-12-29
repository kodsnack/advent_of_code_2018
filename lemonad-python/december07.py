"""
December 07, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def find_next_task(stack, completed, G, G_rev):
        current = None
        s = stack
        for i, c in enumerate(s):
            if c in G_rev:
                found = False
                for c2 in G_rev[c]:
                    if not c2 in completed:
                        found = True
                        break
                if found:
                    continue
            current = c
            del stack[i]
            break
        return current

    @staticmethod
    def path_workers(G, G_rev, alpha_time=False, n_workers=1, extra_time=0):
        stack = sorted(list(set(list(G.keys()) + list(G_rev.keys()))))
        p = []
        completed = []
        wip = {}
        for n in range(n_workers):
            wip[n] = None

        seconds = 0
        while True:
            for w in wip.keys():
                if not wip[w]:
                    task = Solver.find_next_task(stack, completed, G, G_rev)
                    if task:
                        task_time = 1 + extra_time
                        if alpha_time:
                            task_time += ord(task) - ord("A")
                        wip[w] = (task_time, task)

            any_tasks = False
            for w in wip.keys():
                if wip[w]:
                    any_tasks = True
                    remaining_time, task = wip[w]
                    if remaining_time <= 1:
                        p.append(task)
                        completed.append(task)
                        wip[w] = None
                    else:
                        wip[w] = (remaining_time - 1, task)

            if not any_tasks and not stack:
                break
            seconds += 1
        return "".join(p), seconds

    def create_graph(self):
        G = {}
        G_rev = {}
        for m in self.lines_search(
            r"Step (\w) must be finished before step (\w) can begin."
        ):
            before = m.group(1)
            after = m.group(2)
            if before in G:
                G[before].append(after)
                G[before].sort()
            else:
                G[before] = [after]

            if after in G_rev:
                G_rev[after].append(before)
                G_rev[after].sort()
            else:
                G_rev[after] = [before]
        return G, G_rev

    def solve_part_one(self):
        """Solution for part one."""
        G, G_rev = self.create_graph()
        p, n = self.path_workers(G, G_rev)
        return p

    def solve_part_two(self, n_workers=5, extra_time=60):
        """Solution for part two."""
        G, G_rev = self.create_graph()
        p, t = self.path_workers(
            G, G_rev, alpha_time=True, n_workers=n_workers, extra_time=extra_time
        )
        return t

    def solve(self, n_workers=5, extra_time=60):
        return (
            self.solve_part_one(),
            self.solve_part_two(n_workers=n_workers, extra_time=extra_time),
        )


if __name__ == "__main__":
    s = Solver(from_file="input/december07.input")
    one, two = s.solve()
    print("Completion order of steps in the instructions:", one)
    print("How long to complete all of the steps?", two)
