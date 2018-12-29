"""
December 22, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import re

import numpy as np

from common.puzzlesolver import PuzzleSolver
from common.gridgraph import GridGraph


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def get_geo(target_x, target_y, p, depth):
        geologic_index = np.zeros((target_y + p, target_x + p))
        erosion = np.zeros((target_y + p, target_x + p))
        cave = np.zeros((target_y + p, target_x + p), dtype="c")
        cave_f = np.zeros((target_y + p, target_x + p), "int32")

        for y in range(target_y + p):
            for x in range(target_x + p):
                if (x == 0 and y == 0) or (x == target_x and y == target_y):
                    geologic_index[y, x] = 0
                elif y == 0:
                    geologic_index[y, x] = x * 16807
                elif x == 0:
                    geologic_index[y, x] = y * 48271
                else:
                    geologic_index[y, x] = erosion[y, x-1] * erosion[y-1, x]

                erosion[y, x] = (geologic_index[y, x] + depth) % 20183
                if (erosion[y, x] % 3) == 0:
                    cave[y, x] = b'.'
                    cave_f[y, x] = 0
                elif (erosion[y, x] % 3) == 1:
                    cave[y, x] = b'='
                    cave_f[y, x] = 1
                else:
                    cave[y, x] = b'|'
                    cave_f[y, x] = 2
        cave[0, 0] = b'M'
        cave[target_y, target_x] = b'T'
        return geologic_index, erosion, cave, cave_f

    def solve_part_one(self):
        """Solution for part one."""
        m = re.search("depth: (\d+)", self.puzzle_input)
        depth = int(m.group(1))
        m = re.search("target: (\d+),(\d+)", self.puzzle_input)
        target_x = int(m.group(1))
        target_y = int(m.group(2))

        p = 1
        _, _, _, cave_f_mat = self.get_geo(
                target_x,
                target_y,
                p,
                depth)
        return np.sum(np.sum(cave_f_mat[0:target_y + 1, 0:target_x + 1]))

    def solve_part_two(self):
        """Solution for part two."""
        m = re.search("depth: (\d+)", self.puzzle_input)
        depth = int(m.group(1))
        m = re.search("target: (\d+),(\d+)", self.puzzle_input)
        target_x = int(m.group(1))
        target_y = int(m.group(2))

        p = 30
        geologic_index_mat, erosion_mat, cave_mat, cave_f_mat = self.get_geo(
                target_x,
                target_y,
                p,
                depth)

        # 3D graph where plane 0 is movements with torch, plane 1 is movement with
        # climbing gear and plane 2 is movement with neither. Switching equipment
        # means moving to another plane.
        G = GridGraph()
        n_plane = (target_y + p) * (target_x + p)
        for y in range(target_y + p):
            for x in range(target_x + p):
                gi = geologic_index_mat[y, x]
                erosion = erosion_mat[y, x]
                cave = cave_mat[y, x]
                cave_f = cave_f_mat[y, x]

                # 0 torch, 1 climbing, 2 neither.
                ix0 = 0 * n_plane + y * (target_x + p) + x
                ix1 = 1 * n_plane + y * (target_x + p) + x
                ix2 = 2 * n_plane + y * (target_x + p) + x
                ix = [ix0, ix1, ix2]

                for z in range(3):
                    if cave_f == 1 and z == 0:
                        # Can't use torch in wet region.
                        continue
                    elif cave_f == 2 and z == 1:
                        # Can't use climbing in narrow region.
                        continue
                    elif cave_f == 0 and z == 2:
                        # Can't use neither in rocky region.
                        continue

                    G.add_node(ix[z],
                               extra={'p': (y, x),
                                      'erosion': erosion,
                                      'geologic_index': gi,
                                      'symbol': cave,
                                      'type': cave_f})
                if cave_f == 0:
                    # In rocky region, can switch between torch and climbing.
                    G.add_edge(ix[0], ix[1], weight=7)
                elif cave_f == 1:
                    # In wet region, can switch between climbing and neither.
                    G.add_edge(ix[1], ix[2], weight=7)
                else:
                    # In narrow region, can switch between torch and neither.
                    G.add_edge(ix[0], ix[2], weight=7)

                moves = []
                # Move left?
                if x > 0:
                    cave_f_move = cave_f_mat[y, x - 1]
                    move0 = ix[0] - 1
                    move1 = ix[1] - 1
                    move2 = ix[2] - 1
                    moves.append((cave_f_move, move0, move1, move2))
                # Move up?
                if y > 0:
                    cave_f_move = cave_f_mat[y - 1, x]
                    move0 = ix[0] - (target_x + p)
                    move1 = ix[1] - (target_x + p)
                    move2 = ix[2] - (target_x + p)
                    moves.append((cave_f_move, move0, move1, move2))

                for cave_f_move, move0, move1, move2 in moves:
                    if cave_f == 0 and cave_f_move == 0:
                        # Rocky -> rocky, can move with both torch and climbing.
                        G.add_edge(ix[0], move0, weight=1)
                        G.add_edge(ix[1], move1, weight=1)
                    elif ((cave_f == 0 and cave_f_move == 1) or
                          (cave_f == 1 and cave_f_move == 0)):
                        # Rocky -> wet, can move with climbing.
                        G.add_edge(ix[1], move1, weight=1)
                    elif ((cave_f == 0 and cave_f_move == 2) or
                          (cave_f == 2 and cave_f_move == 0)):
                        # Rocky -> narrow, can move with torch.
                        G.add_edge(ix[0], move0, weight=1)
                    elif cave_f == 1 and cave_f_move == 1:
                        # Wet -> wet, can move with both climbing and neither.
                        G.add_edge(ix[1], move1, weight=1)
                        G.add_edge(ix[2], move2, weight=1)
                    elif ((cave_f == 1 and cave_f_move == 2) or
                          (cave_f == 2 and cave_f_move == 1)):
                        # Wet -> Narrow, can move with neither.
                        G.add_edge(ix[2], move2, weight=1)
                    else:
                        # Narrow -> Narrow, can move with both torch and neither.
                        G.add_edge(ix[0], move0, weight=1)
                        G.add_edge(ix[2], move2, weight=1)

        tix = target_y * (target_x + p) + target_x
        return G.shortest_path(0, tix)

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december22.input")
    (one, two) = s.solve()
    print("Total risk level:", one)
    print("Fewest number of minutes to reach the target:", two)
