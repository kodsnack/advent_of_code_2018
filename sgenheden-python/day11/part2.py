
import sys

from utils import create_matrix


if __name__ == "__main__":

    serial = int(sys.argv[1])
    mat = create_matrix(serial)

    max_power = -1E6
    max_xy = None
    max_size = None
    for size in range(1, 301):
        print(size)
        sys.stdout.flush()
        for i in range(300-size):
            for j in range(300-size):
                total = mat[i:i+size, j:j+size].sum()
                if max_power < total:
                    max_power = total
                    max_xy = [i+1, j+1]
                    max_size = size

    if max_power < mat.sum():
        max_power = mat.sum()
        max_xy = [1, 1]
        max_size = 300

    print(f"Max power = {max_power} as {max_xy[0]},{max_xy[1]} with size {max_size}")
