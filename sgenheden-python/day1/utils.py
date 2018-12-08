
import fileinput
import numpy as np


def stdin2ndarray() :
    data = []
    for line in fileinput.input() :
        data.append(line.strip())
    return np.array(data,dtype=int)
