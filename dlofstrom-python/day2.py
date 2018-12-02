import sys
from itertools import combinations

# Read input
input = sys.stdin.read()
input = input.strip().split('\n')

# Count of occurring characters for every input row
occurrences = [set([line.count(c) for c in line]) for line in input]
twice = sum([1 for s in occurrences if 2 in s])
thrice = sum([1 for s in occurrences if 3 in s])
print('Part 1: ' + str(twice*thrice))

# Compare every two rows for the pair where only one character differs
for l1,l2 in combinations(input, 2):
    same = [c1==c2 for c1,c2 in zip(l1,l2)]
    # If only one differs
    if same.count(False) == 1:
        line = [c for c,s in zip(l1,same) if s]
        break
print('Part 2: ' + ''.join(line))
