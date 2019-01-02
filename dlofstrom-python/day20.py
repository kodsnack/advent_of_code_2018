import sys
import re
from itertools import product, combinations

# Read input
input = sys.stdin.read().strip()

queue = list(input[1:-1])
stack = [[]]
m = {(0,0):'.'}
x,y = (0,0)
while queue:
    direction = queue.pop(0)
    
    # Take step in direction
    if direction == 'N':
        xd,yd = (x,y-1)
        x,y = (x,y-2)
        m[(x,y)] = '.'
        m[(xd,yd)] = '/'
    elif direction == 'S':
        xd,yd = (x,y+1)
        x,y = (x,y+2)
        m[(x,y)] = '.'
        m[(xd,yd)] = '/'
    elif direction == 'E':
        xd,yd = (x+1,y)
        x,y = (x+2,y)
        m[(x,y)] = '.'
        m[(xd,yd)] = '/'
    elif direction == 'W':
        xd,yd = (x-1,y)
        x,y = (x-2,y)
        m[(x,y)] = '.'
        m[(xd,yd)] = '/'
    elif direction == '(':
        # Add to current level
        stack[-1].append((x,y))
        # Add new stack level
        stack.append([])
    elif direction == '|':
        # Add to current stack level
        stack[-1].append((x,y))
        # Reset position
        x,y = stack[-2][-1]
    elif direction == ')':
        # Add to current stack level
        stack[-1].append((x,y))
        # Remove last element in previous level
        stack[-2].pop()
        # Append current stack to previous level
        stack[-2] += stack[-1]
        # Remove current level
        stack.pop()
            
minx = min([x for x,y in m])
maxx = max([x for x,y in m])
miny = min([y for x,y in m])
maxy = max([y for x,y in m])

# Search paths in maze
def bfs(m, p0, min_length=0):
    queue = [(p0[0],p0[1],0)]
    visited = set(p0)
    far_away = set()
    longest = 0
    while queue:
        x,y,length = queue.pop(0)
        #print(x,y,length)
        
        if length > longest:
            longest = length
        if length >= min_length:
            far_away.add((x,y))

            
        directions = [((x+2,y),(x+1,y)),((x-2,y),(x-1,y)),((x,y+2),(x,y+1)),((x,y-2),(x,y-1))]
        directions = [direction for direction,door in directions if door in m]
        for direction in directions:
            if direction not in visited:
                queue.append((direction[0],direction[1],length+1))
                visited.add(direction)

    if min_length != 0:
        return len(far_away)
    else:
        return longest


print('Part 1:', bfs(m,(0,0)))
print('Part 2:', bfs(m,(0,0), 1000))


