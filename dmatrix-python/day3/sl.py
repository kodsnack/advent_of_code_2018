from collections import defaultdict

# Part 1
def parse(line):
    xy = line.split()[2].rstrip(':').split(',')
    size = line.split()[3].split('x')
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
def parse(line):
    id = line.split()[0].lstrip('#')
    xy = line.split()[2].rstrip(':').split(',')
    size = line.split()[3].split('x')
    return int(id), int(xy[0]), int(xy[1]), int(size[0]), int(size[1])

dd = defaultdict(int)
with open('input.txt') as f:
    my_lines = f.readlines()
    for line in my_lines:
        id, sx, sy, width, height = parse(line)
        for x in range(width):
            for y in range(height):
                dd[(sx + x, sy + y)] += 1

    for line in my_lines:
        id, sx, sy, width, height = parse(line)
        no_overlap = True
        for x in range(width):
            for y in range(height):
                if dd[(sx + x, sy + y)] > 1:
                   no_overlap = False
        if no_overlap:  
            print(f"Answer for the second part: {id}")
            break
