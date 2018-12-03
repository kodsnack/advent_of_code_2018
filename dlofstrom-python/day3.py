import sys
import re
from itertools import product

# Read input
input = sys.stdin.read()
input = input.strip().split('\n')
input = [[int(n) for n in re.findall('\d+',l)] for l in input]

# Create a dictionary with every square inch of fabric and the overlap count
fabric = {}
for id,x,y,sx,sy in input:
    # All square inch patches in this area
    patches = product(range(x,x+sx),range(y,y+sy))
    for p in patches:
        # Calculate overlap
        if p not in fabric:
            fabric[p] = 0
        else:
            fabric[p] += 1
# Count all patches with overlaps
overlapping = sum(1 for k in fabric if fabric[k] > 0)
print('Part 1: ' + str(overlapping))

# Check for every area if no patches overlap
for id,x,y,sx,sy in input:
    # All square inch patches in this area
    patches = product(range(x,x+sx),range(y,y+sy))
    # Done if no overlapping
    if sum(1 for p in patches if fabric[p] > 0) == 0:
        break
print('Part 2: ' + str(id))
