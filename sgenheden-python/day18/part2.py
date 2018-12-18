
import numpy as np

from utils import read_input, update_grid, print_grid, score_grid


if __name__ == "__main__":
    grid = read_input()
    original_grid = np.array(grid)
    total = 1000000000

    visited = {}
    cycle_start = None
    cycle_end = None

    for i in range(total):
        grid = update_grid(grid)
        grid_str = ''.join(''.join(row) for row in grid)
        if grid_str in visited:
            cycle_start = visited[grid_str]
            cycle_end = i
            break

        visited[grid_str] = i

    period = cycle_end - cycle_start
    if total >= cycle_start:
        total = cycle_start + (total - cycle_start) % period
    print(f"Cycle starts at {cycle_start} and ends at {cycle_end} with a period of {period}")
    print(f"Will run {total} cycles to obtain the desired result")

    grid = original_grid
    for _ in range(total):
        grid = update_grid(grid)

    print_grid(grid)
    ntree, nlumberjard, ntotal = score_grid(grid)
    print(f"There are {ntree} trees and {nlumberjard} for a value of {ntotal}")
