import sys
import re
from itertools import product, combinations

# Read input
input = sys.stdin.read().strip()
input = input.split('\n')
input = [re.findall('-?\d+',l) for l in input]
input = [[int(x) for x in l] for l in input]

strongest = max(input, key=lambda x: x[3])
xs,ys,zs,rs = strongest

s = 0
for x,y,z,r in input:
    d = abs(xs-x)+abs(ys-y)+abs(zs-z)
    if d <= rs:
        s += 1
print('Part 1:', s)


# Get diamond edges
points = set()
for x,y,z,r in input:
    points.add((x-r,y,z))
    points.add((x+r,y,z))
    points.add((x,y-r,z))
    points.add((x,y+r,z))
    points.add((x,y,z-r))
    points.add((x,y,z+r))

# Check how many nanobots can see a point
def in_range(xs,ys,zs):
    s = 0
    for x,y,z,r in input:
        d = abs(xs-x)+abs(ys-y)+abs(zs-z)
        if d <= r:
            s += 1
    return s
    
points_in_range = {(x,y,z):in_range(x,y,z) for x,y,z in points}
best_score = max(points_in_range.values())
points_filtered = [(x,y,z) for x,y,z in points if points_in_range[(x,y,z)] == best_score]
best_point = points_filtered.pop(0)
directions = [(1,1,0),(-1,1,0),(1,-1,0),(-1,-1,0),(1,0,1),(-1,0,1),(1,0,-1),(-1,0,-1),(0,1,1),(0,-1,1),(0,1,-1),(0,-1,-1)]

best = 0
best_closest = 0
for xd,yd,zd in directions:
    score = best_score
    closest = sum(best_point)
    x,y,z = best_point
    count = 0
    while True:
        x += xd
        y += yd
        z += zd

        s = in_range(x,y,z)

        if s < score:
            break
        elif s > score:
            score = s
            closest = abs(x)+abs(y)+abs(z)
        else:
            d = abs(x)+abs(y)+abs(z)
            if d < closest:
                closest = d

        if count > 1000:
            #print('terminated')
            break
        
        count += 1
    if score > best:
        best = score
        best_closest = closest
    #print(xd,yd,zd,x,y,z,s,score,closest)
    
print('Part 2:', best_closest)


