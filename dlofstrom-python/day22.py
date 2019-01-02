import sys
import re
from itertools import product, combinations
import math

# Read input
input = sys.stdin.read().strip()
input = input.split('\n')
input = [l.split(' ') for l in input]

depth = int(input[0][1])
target = tuple(map(int, input[1][1].split(',')))
erosion_levels = {}

def calculate_erosion_level(p):
    x,y = p
    if p in erosion_levels:
        return erosion_levels[p]
    elif p == (0,0):
        erosion_levels[p] = depth % 20183
        return erosion_levels[p]
    elif p == target:
        erosion_levels[p] = depth % 20183
        return erosion_levels[p]
    elif y == 0:
        erosion_levels[p] = (x * 16807 + depth) %  20183
        return erosion_levels[p]
    elif x == 0:
        erosion_levels[p] = (y * 48271 + depth) % 20183
        return erosion_levels[p]
    else:
        erosion_levels[p] = (calculate_erosion_level((x-1,y)) * calculate_erosion_level((x,y-1)) + depth) % 20183
        return erosion_levels[p]

def region_type(p):
    elm = calculate_erosion_level(p) % 3
    return elm


# Risk level
s = 0
for y in range(target[1]+1):
    for x in range(target[0]+1):
        s += region_type((x,y))
print('Part 1:', s)

tools = {'neither':{1,2}, 'climbing_gear':{0,1}, 'torch':{0,2}}
tools_region = {0:{'climbing_gear','torch'}, 1:{'climbing_gear','neither'}, 2:{'torch','neither'}}
tools_switch = {(0,'climbing_gear'):'torch', (0,'torch'):'climbing_gear', (1,'climbing_gear'):'neither', (1,'neither'):'climbing_gear', (2,'torch'):'neither', (2,'neither'):'torch'}

def astar(p0,t0,target):
    queue = [(p0[0],p0[1],t0,0,sum(target))]
    visited = set()
    count = 0

    while queue:
        x,y,tool,time,cost = queue.pop(0)

        if (x,y,tool) not in visited:
            visited.add((x,y,tool))
                    
            if (x,y) == target and tool == 'torch':
                return x,y,tool,time,cost
        
            directions = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]
            directions = [d for d in directions if d[0] >= 0 and d[1] >= 0]# and d[0] <= depth and d[1] <= depth]
            directions_direct = [(d,tool,time+1) for d in directions if region_type(d) in tools[tool]]
            
            new_tool = tools_switch[(region_type((x,y)),tool)]
            directions_switch = [((x,y),new_tool,time+7)]
            
            for new_position,new_tool,new_time in directions_direct + directions_switch:
                if new_position+(new_tool,) not in visited:
                    # Heuristic manhattan
                    new_cost = abs(new_position[0]-target[0]) + abs(new_position[1]-target[1])

                    # Insert in queue based on time and heuristic
                    i = 0
                    while i < len(queue) and queue[i][4] < new_time+new_cost:
                        i += 1
                    queue.insert(i,((new_position[0],new_position[1], new_tool, new_time,new_time+new_cost)))
                            
print('Part 2:', astar((0,0),'torch',target)[3])


