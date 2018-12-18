
import sys

from utils import create_matrix


if __name__ == "__main__":

    serial = int(sys.argv[1])
    mat = create_matrix(serial)


    max_power = -1E6
    max_xy = None
    for i in range(300-3):
        for j in range(300-3):
            total = mat[i:i+3, j:j+3].sum()
            if max_power < total:
                max_power = total
                max_xy = [i+1, j+1]
    print(f"Max power = {max_power} as {max_xy[0]},{max_xy[1]}")
