"""
December 10, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import numpy as np
from matplotlib import pyplot as plt
from scipy.spatial.distance import pdist

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)
        self.read_input()

    def read_input(self):
        pos = np.zeros((0, 2), dtype="int")
        vel = np.zeros((0, 2), dtype="int")
        for m in self.lines_search(
            "position=<\s*(-*\d+),\s*(-*\d+)> velocity=<\s*(-*\d+),\s*(-*\d+)>"
        ):
            posx = int(m.group(1))
            posy = int(m.group(2))
            velx = int(m.group(3))
            vely = int(m.group(4))
            pos = np.append(pos, np.array([[posx, posy]]), axis=0)
            vel = np.append(vel, np.array([[velx, vely]]), axis=0)
        self.pos = pos
        self.vel = vel

    def solve(self, plot=False):
        pos = self.pos
        vel = self.vel

        # Fast forward.
        t = 10000
        pos += vel * 10000

        # Find position with minimum pairwise distance for approximate
        # time of message constellation.
        last_dist = 10000000000000
        while True:
            dist = sum(pdist(pos, "euclidean"))
            if (last_dist - dist) <= 0:
                break
            last_dist = dist
            pos += vel

            t += 1

        # Move back a few time steps.
        t -= 5
        pos -= vel * 5

        # Find minimum unique x + y distances (should be when characters
        # line up). Can't only use this method as we don't know when to
        # stop.
        min_ul = None
        min_pos = None
        min_i = None
        for i in range(t, t + 11):
            ul = len(np.unique(pos[:, 0])) + len(np.unique(pos[:, 1]))
            if min_ul is None or ul <= min_ul:
                min_ul = ul
                min_pos = pos.copy()
                min_i = i
            pos += vel

        # Show plot.
        if plot:
            print("See plot window")
            plt.plot(min_pos[:, 0], -min_pos[:, 1], "bo")
            plt.title("%d" % min_i)
            plt.show()
        return min_i


if __name__ == "__main__":
    s = Solver(from_file="input/december10.input")
    two = s.solve(plot=True)
    print("Seconds to wait for message to appear:", two)
