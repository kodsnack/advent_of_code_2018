import sys
import re

# Read input
input = sys.stdin.read().strip()
input = input.split('\n')
input = [re.findall('-?\d+',l) for l in input]
input = [[int(x) for x in l] for l in input]

position = [(x,y) for x,y,vx,vy in input]
velocity = [(vx,vy) for x,y,vx,vy in input]

# Get edges of bounding box for points
def edges(position):
    xmax = max(position, key=lambda t:t[0])[0]
    ymax = max(position, key=lambda t:t[1])[1]
    xmin = min(position, key=lambda t:t[0])[0]
    ymin = min(position, key=lambda t:t[1])[1]
    return xmin,xmax,ymin,ymax

# Move points
def move(pos,vel):
    new_position = []
    for p,v in zip(pos,vel):
        new_position.append((p[0]+v[0],p[1]+v[1]))
    return new_position

# Pretty print
def show(pos,s):
    xmin,xmax,ymin,ymax = s
    a = [['.' for x in range(xmin,xmax+1)] for y in range(ymin,ymax+1)]
    for x,y in pos:
        a[y-ymin][x-xmin] = '#'
    for l in a:
        print(''.join(l))


# Starting area
xmin,xmax,ymin,ymax = edges(position)
area_new = (xmax-xmin)*(ymax-ymin)
area = area_new+1
time = 0

# Loop as long as area decreases
while area_new < area:
    area = area_new
    # Move particles and update area
    position_new = move(position,velocity)
    xmin,xmax,ymin,ymax = edges(position_new)
    area_new = (xmax-xmin)*(ymax-ymin)
    if area_new < area:
        position = position_new
        time += 1

print('Part 1:')
show(position,edges(position))
print('Part 2:', time)
