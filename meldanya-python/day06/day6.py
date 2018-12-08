import sys
from collections import defaultdict, namedtuple

Point = namedtuple("Point", ["x", "y"])


def mh_dist(p1, p2):
    return abs(p1.x - p2.x) + abs(p1.y - p2.y)


with open(sys.argv[1]) as f:
    points = [
        Point(*tuple(int(c) for c in l.strip().split(","))) for l in f.readlines()
    ]

SIZE = 360


def find_closest_point(points, x, y):
    dists = {}
    for p in points:
        dists[p] = mh_dist(p, Point(x, y))

    # Check if there's a single closest point or multiple.
    point, min_ = min(dists.items(), key=lambda x: x[1])
    for p, dist in dists.items():
        if dist == min_ and point != p:
            # Found multiple closest points, return None
            return None
    return point


lt10000 = 0
grid = [[None for _ in range(SIZE)] for _ in range(SIZE)]
for y, r in enumerate(grid):
    for x, c in enumerate(r):
        closest = find_closest_point(points, x, y)
        mh_dist_sum = 0
        for pi, p in enumerate(points):
            mh_dist_sum += mh_dist(p, Point(x, y))
            if p.x == x and p.y == y or closest and closest == p:
                # Mark each grid point with the closest point.
                grid[y][x] = p
        if mh_dist_sum < 10000:
            lt10000 += 1


def find_inf(grid):
    inf = set()
    for y, r in enumerate(grid):
        for x, c in enumerate(r):
            # Regions are infinite if they are at one edge of the grid.
            if y == 0 or x == 0 or y == SIZE - 1 or x == SIZE - 1:
                inf.add(c)
    return inf


# Iterate through grid and count region sizes which are not infinite.
not_inf = set(points) - find_inf(grid)
sizes = defaultdict(int)
for r in grid:
    for c in r:
        for p in not_inf:
            if p == c:
                sizes[p] += 1

print(max(sizes.items(), key=lambda x: x[1]))


#
# Part 2
print(lt10000)
