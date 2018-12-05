import sys
import re
from itertools import product, combinations

# Read input
input = sys.stdin.read().strip()

# Execute polymer reaction
def react(p):
    i = 0
    l = len(p)-1
    # Generate possible pairs of units
    pairs = set(c+c.swapcase() for c in p)
    # Go throug polymer and react
    while i < l:
        if p[i:i+2] in pairs:
            # Remove destroyed
            p = p[:i] + p[i+2:]
            l = len(p)-1
            i = max(i-1,0)
        else:
            i += 1
    return p

print('Part 1: ' + str(len(react(input))))

# Remove every a/A unit and react
d = {}
for a in set(input.lower()):
    polymer = re.sub(a+'|'+a.upper(),'',input)
    d[a] = len(react(polymer))
s = min(d, key=lambda k:d[k])
print('Part 2: ' + str(d[s]))
