
from utils import read_input, grow


if __name__ == "__main__":

    low_boundary = -10
    gen, trans = read_input(abs(low_boundary))

    print(gen)
    scores = grow(gen, trans, low_boundary, 20, verbose="gen")

    print(f"Summa = {scores[-1][1]}")
