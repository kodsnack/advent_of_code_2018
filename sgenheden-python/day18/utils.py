

import fileinput
import itertools

import numpy as np


def new_value(grid, x, y):

    counts = {
        ".": 0,
        "|": 0,
        "#": 0
    }
    for dx, dy in itertools.product([0, 1,-1],[0, 1,-1]):
        if dx == 0 and dy == 0:
            continue
        nx = x + dx
        ny = y + dy
        if nx < 0 or nx == grid.shape[0] or ny < 0 or ny == grid.shape[1]:
            continue
        counts[grid[nx, ny]] += 1

    if grid[x, y] == '.':
        return '|' if counts['|'] >= 3 else '.'
    elif grid[x, y] == '|':
        return '#' if counts['#'] >= 3 else '|'
    elif grid[x, y] == '#':
        return '#' if counts['#'] >= 1 and counts['|'] >= 1 else '.'


def print_grid(grid):
    for row in grid:
        print("".join(row))
    print()


def read_input():
    data = []
    for line in fileinput.input():
        data.append(list(line.strip()))
    return np.array(data, dtype=str)


def score_grid(grid):
    ntree = np.sum(grid == '|')
    nlumberjard = np.sum(grid == '#')
    return ntree, nlumberjard, ntree * nlumberjard


def update_grid(grid):
    new_grid = np.zeros(grid.shape, dtype=str)
    for x in range(grid.shape[0]):
        for y in range(grid.shape[1]):
            new_grid[x, y] = new_value(grid, x, y)
    return np.asarray(new_grid)
