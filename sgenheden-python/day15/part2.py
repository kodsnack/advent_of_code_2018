
import sys

from utils import World, ElfLossException


if __name__ == "__main__":

    for power in range(4, 20):
        w = World(sys.argv[1], elves_power=power)
        try:
            n = w.play(verbose=False, elves_loss=True)
        except ElfLossException:
            print(f"Elf lost with power {power}")
        else:
            tot_hit_pts = sum([obj.hit_points for obj in w.goblins_and_elves])
            print(f"Battle with power {power} ended in round {n} with a total of "
                  f"{tot_hit_pts}, solution is {tot_hit_pts*n}")
            break
