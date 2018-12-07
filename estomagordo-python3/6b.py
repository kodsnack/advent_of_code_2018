from collections import defaultdict


def distance(x1, y1, x2, y2):
    return abs(x1 - x2) + abs(y1 - y2)


def find_closest(coords, x, y):
    closest = 1000000
    closestcells = []

    for i, coord in enumerate(coords):
        d = distance(x, y, coord[0], coord[1])
        if d < closest:
            closest = d
            closestcells = [i]
        elif d == closest:
            closestcells.append(i)

    if len(closestcells) > 1:
        return -1
    return closestcells[0]


def solve(d):
    coords = []
    leftest = 100000
    rightest = -1000000
    uppest = 100000
    lowest = -1000000

    for line in d:
        x, y = map(int, line.split(', '))
        coords.append((x, y))
        leftest = min(leftest, x)
        rightest = max(rightest, x)
        uppest = min(uppest, y)
        lowest = max(lowest, y)

    good = 0
    count = 0

    for x in range(leftest - 350, rightest + 350):
        for y in range(uppest - 350, lowest + 350):
            count += 1
            total = 0
            for coord in coords:
                total += distance(x, y, coord[0], coord[1])
            if total < 10000:
                good += 1
                
    return good    

with open('input_6_yxhuvud.txt') as f:
    data = [line.rstrip() for line in f]
    print(solve(data))