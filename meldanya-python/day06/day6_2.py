# Slightly faster implementation
import sys
from collections import namedtuple

Point = namedtuple("Point", ["x", "y"])
SIZE = 360

with open(sys.argv[1]) as f:
    points = [Point(*(int(c) for c in l.strip().split(","))) for l in f.readlines()]


def _find_closest(points, x, y):
    dists = {}
    for p in points:
        dists[p] = abs(p.x - x) + abs(p.y - y)

    # Check if there's a single closest point or multiple.
    point, min_ = min(dists.items(), key=lambda x: x[1])
    for p, dist in dists.items():
        if dist == min_ and point != p:
            # Found multiple closest points, return None
            return None
    return point


def _is_inf(region):
    for x, y in region:
        if x == 0 or y == 0 or y == SIZE - 1 or x == SIZE - 1:
            return True
    return False


grid = {p: set() for p in points}
grid[None] = set()
for x in range(SIZE):
    for y in range(SIZE):
        closest = _find_closest(points, x, y)
        grid[closest].add((x, y))

not_inf = set(p for p, c in grid.items() if not _is_inf(c))
print(max(len(grid[p]) for p in not_inf))


lt10000 = 0
for x in range(SIZE):
    for y in range(SIZE):
        mh_dists = sum(abs(p.x - x) + abs(p.y - y) for p in points)
        if mh_dists < 10000:
            lt10000 += 1
print(lt10000)
