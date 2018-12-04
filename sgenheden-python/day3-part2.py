
import fileinput

import numpy as np


def read_stdin() :
    items = []
    max_x, max_y = 0, 0
    for line in fileinput.input() :
        item = line.strip().split()
        id = item[0][1:]
        start_x, start_y = map(int, item[2][:-1].split(","))
        len_x, len_y = map(int, item[3].split("x"))
        max_x = max(max_x, start_x + len_x + 1)
        max_y = max(max_y, start_y + len_y + 1)
        items.append((start_x, start_y, start_x + len_x, start_y + len_y, id))
    return items, max_x, max_y


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
