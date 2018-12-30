
import fileinput
import sys


turn_operation = {
    0: {
        '^': "<",
        "v": ">",
        "<": "v",
        ">": "^"
    },
    1: {
        '^': "^",
        "v": "v",
        "<": "<",
        ">": ">",
    },
    2: {
        '^': ">",
        "v": "<",
        "<": "^",
        ">": "v",
    }
}

rotation_operation = {
    '\\': {
        '^': '<',
        "v": '>',
        "<": '^',
        ">": 'v',
    },
    '/': {
        '^': '>',
        "v": '<',
        "<": 'v',
        ">": '^',
    },
}

move_operation = {
    "-": {
        '<': [-1, 0],
        '>': [1, 0]
    },
    '|': {
        '^': [0, -1],
        'v': [0, 1]
    },
    '\\': {
        '^': [0, -1],
        "v": [0, 1],
        "<": [-1, 0],
        ">": [1, 0],
    },
    '/': {
        '^': [0, -1],
        "v": [0, 1],
        "<": [-1, 0],
        ">": [1, 0],
    },
    '+': {
        '^': [0, -1],
        "v": [0, 1],
        "<": [-1, 0],
        ">": [1, 0],
    }
}


def read_input():

    world = []
    ncarts = 0
    for line in fileinput.input():
        line = line[:-1]  # Remove \n
        row = []
        for col in line:
            if col in ["^", "v", "<", ">"]:
                road = "|" if col in ["^", "v"] else "-"
                row.append((road, (col, 0)))
                ncarts += 1
            else:
                row.append(col)
        world.append(row)
    return world, ncarts


def print_world(world):
    for y, row in enumerate(world, 1):
        for x, col in enumerate(row, 1):
            if isinstance(col, tuple):
                sys.stdout.write(f" {col[1][0]} ")
            else:
                sys.stdout.write(f" {col} ")
        sys.stdout.write("\n")
    print("\n\n")

