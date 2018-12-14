
import numpy as np

from utils import read_stdin


if __name__ == "__main__":

    items, xlen, ylen = read_stdin()
    mat = np.zeros([xlen , ylen , len(items)])
    for i, item in enumerate(items):
        mat[item[0]:item[2], item[1]:item[3], i] = 1

    overlap_mat = mat.sum(axis=2)
    for i, item in enumerate(items):
        cond = mat[:, :, i] == 1
        if np.all(overlap_mat[cond] == 1):
            print(f"Item {item[4]} is intact")
