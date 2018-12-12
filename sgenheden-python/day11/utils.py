
import numpy as np


def power_level(x, y, serial):
    rack_id = x + 10
    level = rack_id * y
    level += serial
    level *= rack_id
    lstr = str(level)
    if len(lstr) >= 3:
        hundred_digit = int(lstr[-3:-2])
    else:
        hundred_digit = 0
    return hundred_digit - 5


def create_matrix(serial):

    mat = np.zeros([300, 300])
    for i in range(300):
        for j in range(300):
            mat[i, j] = power_level(i+1, j+1, serial)

    return mat
