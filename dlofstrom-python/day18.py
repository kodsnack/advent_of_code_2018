import sys
import re
from itertools import combinations, product

# Read input
input = sys.stdin.read().strip()

state = input
states = {}
minutes = 0
while True:
    minutes += 1
    state = state.split('\n')
    new_state = [['.' for j in range(len(state[0]))] for i in range(len(state))]
    for x,y in product(range(len(state[0])),range(len(state))):
        # Adjacent acres (plus current)
        adjacent = product((x-1,x,x+1),(y-1,y,y+1))
        adjacent = [a for a in adjacent if a[0] >= 0 and a[0] < len(state) and a[1] >= 0 and a[1] < len(state)]
        adjacent = [state[a[1]][a[0]] for a in adjacent]

        # Current acre and surrounding acres
        s = state[y][x]
        o = adjacent.count('.')
        t = adjacent.count('|')
        l = adjacent.count('#')
        # If current acre is open and 3 or more trees adjacent
        if s == '.' and t >= 3:
            new_state[y][x] = '|'
        # If current acre is trees and 3 or more lumberyards adjacent
        elif s == '|' and l >= 3:
            new_state[y][x] = '#'
        # If current acre is lumberyard and 1 or more (not current) lumberyards and 1 or more trees adjacent
        elif s == '#' and l > 1 and t > 0:
            new_state[y][x] = '#'
        # If current acre is lumberyard
        elif s == '#':
            new_state[y][x] = '.'
        # Else stay the same
        else:
            new_state[y][x] = state[y][x]
    state = '\n'.join([''.join(r) for r in new_state])

    # When first repeating state, record periodlength and minute offset
    if state in states:
        #print(1)
        period_offset = states[state]
        period_length = minutes - states[state]
        break
    states[state] = minutes

# Invert states for minute lookup
tree_states = {v:k for k,v in states.items()}

# After 10 minutes
state = tree_states[10]
trees = state.count('|')
lumberyards = state.count('#')
print('Part 1:', trees*lumberyards)

        
# One billion minutes is the same pattern as
obm = period_offset + ((1000000000 - period_offset) % period_length)
state = tree_states[obm]
trees = state.count('|')
lumberyards = state.count('#')
print('Part 2:', trees*lumberyards)
