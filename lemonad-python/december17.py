"""
December 17, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import re

import numpy as np

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)
        self.DEBUG = False
        np.set_printoptions(threshold=np.nan)
        self.formatter = lambda x: "%s" % x

    def fill_vert(self, x, y, max_x, max_y, G, depth=0):
        if self.DEBUG:
            print("fill_vert {:d} {:d} (depth {:d})".format(x, y, depth))
        while True:
            if y > max_y:
                break
            if G[y, x] == b"|":
                break
            if G[y, x] == b"#" or G[y, x] == b"~":
                # fill horizontally
                self.fill_horiz(x, y - 1, max_x, max_y, G, depth)
                break
            G[y, x] = "|"
            y += 1

    def fill_horiz(self, x, y, max_x, max_y, G, depth=0):
        if self.DEBUG:
            print("fill_horiz {:d} {:d} (depth {:d})".format(x, y, depth))
        # left
        xp1 = x - 1
        xp1v = False
        while True:
            if xp1 < 0:
                break
            if G[y + 1, xp1] != b"#" and G[y + 1, xp1] != b"~":
                xp1v = True
                break
            if G[y, xp1] == b"#":
                xp1 += 1
                break
            xp1 -= 1

        # right
        xp2 = x + 1
        xp2v = False
        while True:
            if xp2 > max_x:
                break
            if G[y + 1, xp2] != b"#" and G[y + 1, xp2] != b"~":
                xp2v = True
                break
            if G[y, xp2] == b"#":
                xp2 -= 1
                break
            xp2 += 1

        if not xp1v and not xp2v:
            G[y, xp1 : xp2 + 1] = "~"
        if xp1v:
            if self.DEBUG:
                print("Vertical fill 1", y)
            G[y, xp1 : xp2 + 1] = "|"
            self.fill_vert(xp1, y + 1, max_x, max_y, G, depth + 1)
        if xp2v:
            if self.DEBUG:
                print("Vertical fill 2", y)
            G[y, xp1 : xp2 + 1] = "|"
            self.fill_vert(xp2, y + 1, max_x, max_y, G, depth + 1)

        # if x > max_x:
        #     return
        # if G[y, x] == b'#' or G[y, x] == '~':
        #     return
        # if G[y, x + 1] != b'#' or G[y, x + 1] != b'~':
        #     return Solver.fill_vert(x + 1, y, max_x, max_y, G)
        # if G[y, x - 1] != b'#' or G[y, x - 1] != b'~':
        #     Solver.fill_vert(x - 1, y, max_x, max_y, G)

    def water(self):
        max_x = 0
        max_y = 0
        min_x = 10000000
        min_y = 10000000
        pairs = []
        for line in self.lines():
            mx = re.search("x=(\d+),", line)
            if mx:
                x = int(mx.group(1))
                my = re.search("y=(\d+)\.\.(\d+)", line)
                y = range(int(my.group(1)), int(my.group(2)) + 1)
            else:
                my = re.search("y=(\d+),", line)
                y = int(my.group(1))
                mx = re.search("x=(\d+)\.\.(\d+)", line)
                x = range(int(mx.group(1)), int(mx.group(2)) + 1)
            pairs.append((x, y))

            max_x = np.max([np.max(x), max_x])
            max_y = np.max([np.max(y), max_y])
            min_x = np.min([np.min(x), min_x])
            min_y = np.min([np.min(y), min_y])

        min_x -= 1
        max_x += 1
        G = np.zeros((max_y - min_y + 2, max_x - min_x + 2), dtype="c")
        G.fill(b".")

        tap_x = 500 - min_x + 1
        G[0, tap_x] = b"+"

        for p in pairs:
            x = p[0] - min_x + 1
            y = p[1] - min_y + 1
            G[y, x] = b"#"

        max_x -= min_x - 1
        max_y -= min_y - 1
        min_x = 0
        min_y = 0

        if self.DEBUG:
            print(
                np.array2string(
                    np.char.decode(G),
                    separator="",
                    formatter={"str_kind": self.formatter},
                )
            )
            print()

        iters = 0
        last_water = 1
        while True:
            G[G == b"|"] = "."
            self.fill_vert(tap_x, 1, max_x, max_y, G)
            iters += 1
            if self.DEBUG:
                print(
                    np.array2string(
                        np.char.decode(G),
                        separator="",
                        formatter={"str_kind": self.formatter},
                    )
                )
                print()
            water = np.sum(G == b"|")
            water += np.sum(G == b"~")
            if last_water == water:
                break
            last_water = water
        return last_water, np.sum(G == b"~")

    def solve(self):
        n_water_tiles, retained_tiles = self.water()
        return n_water_tiles, retained_tiles


if __name__ == "__main__":
    s = Solver(from_file="input/december17.input")
    (one, two) = s.solve()
    print("Tiles can the water reach:", one)
    print("Retained tiles:", two)
