import sys
from itertools import product, combinations
from datetime import datetime
import re

# Read input
input = sys.stdin.read().strip()
sn = int(input)

# Power level for one cell
def power_level(x, y, serial_number):
    rid = x + 10
    pl = rid * y
    pl = pl + serial_number
    pl = pl * rid
    pl = int(pl/100) % 10
    pl -= 5
    return pl

# Calculate each cell power level
power_levels = {(x,y):power_level(x,y,sn) for x,y in product(range(1,301), repeat=2)}

# Calculate power level of square size 3x3
power_levels_3 = {(x,y):sum([power_levels[c] for c in product(range(x,x+3),range(y,y+3))]) for x,y in product(range(1,301-3), repeat=2)}
max_key = max(power_levels_3, key=lambda k:power_levels_3[k])
print('Part 1:', ','.join([str(x) for x in max_key]))


# Calculate first layer of rows and columns
x2xs = {(x,y,1):power_levels[(x,y)] for x,y in power_levels}
y2ys = {(x,y,1):power_levels[(x,y)] for x,y in power_levels}

# Calculate rows and columns for lookup
for s in range(2,301):
    for x,y in product(range(1,301-s),range(s,301)):
        x2xs[(x,y,s)] = x2xs[(x,y,s-1)] + power_levels[(x+s-1,y)]
    for x,y in product(range(s,301),range(1,301-s)):
        y2ys[(x,y,s)] = y2ys[(x,y,s-1)] + power_levels[(x,y+s-1)]

# Calculate first layer of size sums (size = 1)
power_levels_s = {(x,y,1):power_levels[(x,y)] for x,y in power_levels}

# Calculate sum of power levels in square
for s in range(2,301):
    for x,y in product(range(1,301-s), repeat=2):
        power_levels_s[(x,y,s)] = power_levels_s[(x,y,s-1)]+x2xs[(x,y+s-1,s-1)]+y2ys[(x+s-1,y,s)]

max_key = max(power_levels_s, key=lambda k:power_levels_s[k])
print('Part 2:', ','.join([str(x) for x in max_key]))
