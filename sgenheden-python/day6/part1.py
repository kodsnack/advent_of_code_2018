
import numpy as np
from scipy.spatial.distance import cityblock

from utils import stdin2ndarray


if __name__ == "__main__":

    coordinates = stdin2ndarray()

    min_x = coordinates[:,0].min()
    min_y = coordinates[:,1].min()
    max_x = coordinates[:,0].max()
    max_y = coordinates[:,1].max()

    counter = [0]*len(coordinates)
    for x in range(min_x, max_x+1):
        for y in range(min_y, max_y+1):
            d = np.array([cityblock([x, y], c) for c in coordinates])
            nmin = np.sum(d == d.min())
            if nmin > 1:
                continue
            counter[int(np.argmin(d))] += 1 if not( x == min_x or y == min_y or x == max_x or y == max_y) else 1e6

    counter = np.array(counter, dtype=int)
    print(f"Maximum area is {counter[counter<1e6].max()}")
