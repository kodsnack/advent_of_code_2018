from collections import defaultdict
from typing import Tuple


# Part 1
def parse(input_line: str) -> Tuple[int, int, int, int]:
    xy = input_line.split()[2].rstrip(':').split(',')
    size = input_line.split()[3].split('x')
    return int(xy[0]), int(xy[1]), int(size[0]), int(size[1])


dd = defaultdict(int)
with open('input.txt') as f:
    my_lines = f.readlines()
    for line in my_lines:
        sx, sy, width, height = parse(line)
        for x in range(width):
            for y in range(height):
                dd[(sx + x, sy + y)] += 1
    print(f"Answer for the first part: {sum(v > 1 for k, v in dd.items())}")


# Part 2
def parse(input_line: str) -> Tuple[int, int, int, int, int]:
    idt_prs = input_line.split()[0].lstrip('#')
    xy = input_line.split()[2].rstrip(':').split(',')
    size = input_line.split()[3].split('x')
    return int(idt_prs), int(xy[0]), int(xy[1]), int(size[0]), int(size[1])


dd = defaultdict(int)
with open('input.txt') as f:
    my_lines = f.readlines()
    for line in my_lines:
        idt, sx, sy, width, height = parse(line)
        for x in range(width):
            for y in range(height):
                dd[(sx + x, sy + y)] += 1

    for line in my_lines:
        idt, sx, sy, width, height = parse(line)
        no_overlap = True
        for x in range(width):
            for y in range(height):
                if dd[(sx + x, sy + y)] > 1:
                    no_overlap = False
        if no_overlap:
            print(f"Answer for the second part: {idt}")
            break
