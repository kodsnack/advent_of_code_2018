"""
December 20, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from collections import deque
import heapq

import numpy as np

from common.puzzlesolver import PuzzleSolver
from common.gridgraph import GridGraph


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def parse_inner_directions_g(parent_x, parent_y, dir_list, start_pos, end_pos, G):
        parent_node_id = (parent_x, parent_y)
        out_len = 0
        pos = start_pos
        current_node_id = parent_node_id
        x = parent_x
        y = parent_y

        while pos <= end_pos:
            if dir_list[pos] in ["N", "E", "S", "W"]:
                if dir_list[pos] == "N":
                    y -= 1
                elif dir_list[pos] == "S":
                    y += 1
                elif dir_list[pos] == "E":
                    x += 1
                else:
                    x -= 1

                node_id = (x, y)
                if not G.get_node(node_id):
                    G.add_node(node_id, extra={"symbol": dir_list[pos], "pos": (x, y)})
                G.add_edge(current_node_id, node_id)
                current_node_id = node_id
            elif dir_list[pos] == "|":
                current_node_id = parent_node_id
                x = parent_x
                y = parent_y
            elif dir_list[pos] == "(":
                pos += 1
                inner_start_pos = pos
                level = 1
                while level > 0:
                    if dir_list[pos] == "(":
                        level += 1
                    if dir_list[pos] == ")":
                        level -= 1
                    pos += 1
                pos -= 1  # end on ')'.
                inner_end_pos = pos
                Solver.parse_inner_directions_g(
                    x, y, dir_list, inner_start_pos, inner_end_pos, G
                )
            pos += 1
        return None

    def path_lengths(self):
        G = GridGraph()
        G.add_node((0, 0), extra={"symbol": "X", "dist": 0})

        Solver.parse_inner_directions_g(
            0, 0, self.puzzle_input, 1, len(self.puzzle_input) - 1, G
        )
        return G.all_shortest_paths((0, 0))

    def solve_part_one(self):
        """Solution for part one."""
        return max(list(self.path_lengths()))

    def solve_part_two(self):
        """Solution for part two."""
        cnt = 0
        for pathlen in self.path_lengths():
            if pathlen >= 1000:
                cnt += 1
        return cnt

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december20.input")
    (one, two) = s.solve()
    print("Largest number of doors to reach a room:", one)
    print(">=1000 doors needed to reach rooms:", two)
