
import fileinput
import re

import numpy as np


def parse_input():

    coordinates = []
    velocities = []
    for line in fileinput.input():
        m = re.match("position=<(.*),(.*)> velocity=<(.*),(.*)>", line)
        coordinates.append([int(m.group(1)), int(m.group(2))])
        velocities.append([int(m.group(3)), int(m.group(4))])
    return np.asarray(coordinates), np.asanyarray(velocities)


def calc_boundary(coordinates):
    min_x = coordinates[:, 0].min()
    min_y = coordinates[:, 1].min()
    max_x = coordinates[:, 0].max()
    max_y = coordinates[:, 1].max()
    len_x = max_x - min_x + 1
    len_y = max_y - min_y + 1
    return [min_x, min_y], [len_x, len_y]


if __name__ == "__main__":

    coordinates0, velocities = parse_input()
    coordinates = np.array(coordinates0)

    min_area = 1E10
    min_i = None
    for i in range(200000): # This upper boundary might have to be modified for other situations
        min_xy, len_xy = calc_boundary(coordinates)
        area = len_xy[0]*len_xy[1]
        if min_area > area:
            min_area = area
            min_i = i
        coordinates = coordinates + velocities

    print(f"Minimum area {min_area} found in {min_i} seconds")
    coordinates = coordinates0 + velocities*min_i
    min_xy, len_xy = calc_boundary(coordinates)
    trans = coordinates - min_xy
    mat = np.zeros(len_xy, dtype=str)
    mat[:,:] = " . "
    mat[trans[:,0], trans[:,1]] = "#"
    for row in mat.T:
        print("".join(row))
