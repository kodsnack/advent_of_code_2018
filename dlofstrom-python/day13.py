import sys
import re
from itertools import combinations

# Read input
input = sys.stdin.read()
input = input.split('\n')
input = [list(l) for l in input]

# Initialize track
init_cart = {'<':'-', '^':'|', '>':'-', 'v':'|'}
carts = {}
track = {}
for x,y,v in [(c,r,a) for r,l in enumerate(input) for c,a in enumerate(l)]:
    if v in init_cart:
        track[(y,x)] = init_cart[v]
        carts[(0,y,x)] = (v,'left')
    elif v != ' ':
        track[(y,x)] = v

# Lookups and rules for carts
right = {'<':'^', '^':'>', '>':'v', 'v':'<'}
left = {'<':'v', '^':'<', '>':'^', 'v':'>'}
straight = {'<':'<', '^':'^', '>':'>', 'v':'v'}
turns = {'left':(left,'straight'), 'straight':(straight,'right'),'right':(right,'left')}
nw_se = {'>':left, '<':left, '^':right, 'v':right}
ne_sw = {'>':right, '<':right, '^':left, 'v':left}
step = {'>':(0,1), '<':(0,-1), '^':(-1,0), 'v':(1,0)}

# Capture first collition
first = True

# Loop over carts
while True:
    # Get oldest cart in top left
    t,y,x = min(carts)
    # Pop its direction and next turn from carts
    direction, next_turn = carts.pop((t,y,x))

    # Move cart
    if track[(y,x)] == '/':
        # Top left or bottom right corner
        turn_lookup = nw_se[direction]
        direction = turn_lookup[direction]
    elif track[(y,x)] == '\\':
        # Top right or bottom left corner
        turn_lookup = ne_sw[direction]
        direction = turn_lookup[direction]
    elif track[(y,x)] == '+':
        # Intersection
        turn_lookup, next_turn = turns[next_turn]
        direction = turn_lookup[direction]
    # Move cart in current direction
    y,x = tuple(a+b for a,b in zip((y,x),step[direction]))

    # Check if cart collides with not yet moved cart
    if (t,y,x) in carts:
        if first:
            print('Part 1:', str(x)+','+str(y))
            first = False
        carts.pop((t,y,x))
    # Check if cart collides with already moved cart
    elif (t+1,y,x) in carts:
        if first:
            print('Part 1:', str(x)+','+str(y))
            first = False
        carts.pop((t+1,y,x))
    # If this was the last cart
    elif not carts:
        print('Part 2:', str(x)+','+str(y))
        break
    # Else add to carts with incremented time
    else:
        carts[(t+1,y,x)] = (direction,next_turn)
