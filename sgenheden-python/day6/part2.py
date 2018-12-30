
import numpy as np
from scipy.spatial.distance import cityblock

from utils import stdin2ndarray


if __name__ == "__main__":

    coordinates = stdin2ndarray()

    min_x = coordinates[:,0].min()
    min_y = coordinates[:,1].min()
    max_x = coordinates[:,0].max()
    max_y = coordinates[:,1].max()

    nwithin = 0
    for x in range(min_x, max_x+1):
        for y in range(min_y, max_y+1):
            d = np.array([cityblock([x, y], c) for c in coordinates])
            if d.sum() < 10000:
                nwithin += 1
    print(f"Area {nwithin}")
