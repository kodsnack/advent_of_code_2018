import sys
from itertools import product, combinations
from datetime import datetime
import re

# Read input
input = sys.stdin.read()
input = input.strip().split('\n')
input = [re.findall('\d+',l) for l in input]

# Generate list of coordinates
coordinates = [tuple(map(int, c)) for c in input]

# Generate grid
xs,ys = zip(*coordinates)
xr = range(min(xs),max(xs)+1)
yr = range(min(ys),max(ys)+1)
grid = list(product(xr,yr))

# Calculate manhattan distance from every coordinate to all points on grid
distances = []
for c in coordinates:
    # Distance along x from coordinate
    xd = [abs(x-c[0]) for x in xr]
    # Distance along y from coordinate
    yd = [abs(y-c[1]) for y in yr]
    # Combine to distance in x and y from coordinate
    distances.append(list(map(sum, product(xd,yd))))
# Transform so that every point on grid holds the distance to all coordinates
distances = list(zip(*distances))

# Calculate which coordinate is exclusively closest to a point on the grid
closest = [coordinates[ds.index(min(ds))] if ds.count(min(ds))==1 else '.' for ds in distances]
# Transform so that every coordinate holds a set of points which it is closest to
closest = {k:set([g for g,c in zip(grid,closest) if c==k]) for k in coordinates}
# If an area intersects with the edge of the grid, the size is infinite
edge = set([p for p in grid if p[0] in [xr[0],xr[-1]] or p[1] in [yr[0],yr[-1]]])

# Get max area that is not infinite
print('Part 1:', max([len(v) for k,v in closest.items() if not v.intersection(edge)]))

# Calculate size of region containing points on grid
# with a total distance to all coordinates of less than limit
limit = 10000
print('Part 2:', sum([1 for ds in distances if sum(ds) < limit]))
