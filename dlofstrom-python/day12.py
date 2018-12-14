import sys
import re
from itertools import product

# Read input
input = sys.stdin.read().strip()
input = input.split('\n')

# Initial state first in input
initial_state = input[0].split(': ')[1]
# Rules for a pot in relation to closeby pots
rules = [l.split(' => ') for l in input[2:]]
rules = {k:v for k,v in rules}

# Use offset to first plant to summarize plant containing pots
def sum_of_pots(state, offset):
    return sum([i+offset for i,x in enumerate(state) if x == '#'])

# Generate next generation of plants
def generate(state, offset):
    # Add some empty pots on each side
    current_state = '....' + state + '....'
    # Generate next generation
    new_state = ''.join([rules[current_state[p-2:p+3]] for p in range(2,len(current_state)-2)])
    # Adjust offset
    offset -= 2

    # Cut empty pots from both ends of line
    start = new_state.index('#')
    end = new_state.rindex('#')
    offset += start
    return (new_state[start:end+1], offset)


# Do 20 generations
state, offset = initial_state, 0
for generation in range(1,21):
    state, offset = generate(state, offset)

# Get sum of number of pots that contain plants
sop = sum_of_pots(state, offset)
print('Part 1:', sop)


# Loop until state stays the same (only offset changes)
old_state, old_sop = '', sop
while state != old_state:
    #for i in range(80):
    # Generate next generation
    old_state, old_sop = state, sop
    state, offset = generate(state, offset)
    sop = sum_of_pots(state, offset)

    # Save difference
    difference = sop - old_sop
    generation += 1

# Calculate what the final sop will be after 50G generations
# using difference between generations and number of generations left
sop = sop + difference*(50000000000-generation)
print('Part 2:', sop)
