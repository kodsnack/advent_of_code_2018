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

    areas = defaultdict(int)

    for x in range(rightest + 1):
        for y in range(lowest + 1):
            closest = find_closest(coords, x, y)
            cx, cy = coords[closest]
            if cx > leftest and cx < rightest and cy > uppest and cy < lowest:
                areas[closest] += 1
    
    for x in range(-10000, 10000):
        closest = find_closest(coords, x, -10000)
        areas[closest] = 0
        closest = find_closest(coords, x, 10000)
        areas[closest] = 0
    for y in range(-10000, 10000):
        closest = find_closest(coords, -10000, y)
        areas[closest] = 0
        closest = find_closest(coords, 10000, y)
        areas[closest] = 0
    
    return max(areas.values())    

def read_and_solve():
	with open('input_6.txt') as f:
		data = [line.rstrip() for line in f]
		return solve(data)

if __name__ == '__main__':
	print(read_and_solve())