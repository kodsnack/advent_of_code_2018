
from utils import read_input, update_grid, print_grid, score_grid


if __name__ == "__main__":
    grid = read_input()
    print_grid(grid)

    for _ in range(10):
        grid = update_grid(grid)

    print_grid(grid)
    ntree, nlumberjard, ntotal = score_grid(grid)
    print(f"There are {ntree} trees and {nlumberjard} for a value of {ntotal}")
