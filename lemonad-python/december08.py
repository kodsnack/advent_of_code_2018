"""
December 08, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def read_node(l, ix, metadata, just_sum=True):
        """Reads node (and all of its child nodes recursively)."""
        (n_children, n_metadata) = l[ix : ix + 2]
        ix += 2

        child_metadata = []
        for c in range(n_children):
            ix = Solver.read_node(l, ix, child_metadata, just_sum)

        node_metadata = l[ix : ix + n_metadata]
        ix += n_metadata

        if just_sum:
            node_sum = sum(child_metadata) + sum(node_metadata)
        elif not n_children:
            node_sum = sum(node_metadata)
        else:
            node_sum = sum(
                [child_metadata[m - 1] for m in node_metadata if m <= n_children]
            )
        metadata.append(node_sum)
        return ix

    def solve_part_one(self):
        """Solution for part one."""
        l = list(self.as_list(split_str=" ", conversion=int))
        metadata = []
        self.read_node(l, 0, metadata)
        return sum(metadata)

    def solve_part_two(self):
        """Solution for part two."""
        l = list(self.as_list(split_str=" ", conversion=int))
        metadata = []
        self.read_node(l, 0, metadata, just_sum=False)
        return sum(metadata)

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december08.input")
    (one, two) = s.solve()
    print("Sum of all metadata entries:", one)
    print("Value of the root node:", two)
