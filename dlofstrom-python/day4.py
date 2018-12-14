import sys
from itertools import product, combinations
from datetime import datetime

# Read input
input = sys.stdin.read()
input = [l[1:].split('] ') for l in input.strip().split('\n')]
input = [(datetime.strptime(d, '%Y-%m-%d %H:%M'),s) for d,s in input]
input = sorted(input, key=lambda x:x[0])

# Create a record of the number of times a guard sleeps on a given minute
record = {}
while input:
    # Get the guard id
    guard = int(input.pop(0)[1].split(' ')[1][1:])
    # Get sleep and wake times until next guard
    while input and input[0][1][0] != 'G':
        t1 = input.pop(0)[0].minute
        t2 = input.pop(0)[0].minute
        # Add sleep times to guard record
        record[guard] = record.get(guard,{})
        record[guard].update({t:record[guard].get(t,0)+1 for t in range(t1,t2)})


# Get the guard who sleep the most
id = max(record, key=lambda g: sum(record[g].values()))
minute = max(record[id], key=lambda t: record[id][t])
print('Part 1: ' + str(id*minute))

# Get the guard who sleep at the same time
id = max(record, key=lambda g: max(record[g].values()))
minute = max(record[id], key=lambda t: record[id][t])
print('Part 2: ' + str(id*minute))
