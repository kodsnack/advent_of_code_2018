import sys
import re
from itertools import product, combinations
import math

# Read input
input = sys.stdin.read().strip()

input = input.split('\n')
input = [re.findall('-?\d+',l) for l in input]
input = [[tuple(int(x) for x in l)] for l in input]

constellations_new = input
l = len(constellations_new)+1

while len(constellations_new) < l:
    l = len(constellations_new)
    constellations = constellations_new
    constellations_new = []
    
    while constellations:
        constellation = constellations.pop(0)

        d = 4
        for coordinate in constellation:
            for other_constellation in constellations_new:
                for c in other_constellation:
                    d = sum([abs(a-b) for a,b in zip(coordinate, c)])
                    if d <= 3:
                        break
                if d <= 3:
                    break
            if d <= 3:
                break
        if d <= 3:
            other_constellation += constellation
        else:
            constellations_new += [constellation]

print('Part 1:', len(constellations_new))
