
import sys

from utils import World


if __name__ == "__main__":
    w = World(sys.argv[1])
    w.print()
    n = w.play(verbose=False)
    tot_hit_pts = sum([obj.hit_points for obj in w.goblins_and_elves])
    print(f"Battle ended in round {n} with a total of {tot_hit_pts}, solution is {tot_hit_pts*n}")
    w.print()
