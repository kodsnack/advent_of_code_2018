import sys
import re

# Read input
input = sys.stdin.read().strip()
input = input.split('\n')
input = [re.findall('[yx]|-?\d+',l) for l in input]
input = [(d1,int(a),d2,int(b),int(c)) for d1,a,d2,b,c in input]

clay = set()
standing_water = set()
running_water = set()
for d1,a,d2,b,c in input:
    for d in range(b,c+1):
        if d1 == 'x':
            clay.add((a,d))
        else:
            clay.add((d,a))
            
max_y = max([y for x,y in clay])
pour = [(500,0)]
fill = []
while pour or fill:
    # Pour until hitting clay or standing water
    while pour:
        x,y = pour.pop(0)
        # End if out of grid
        if y > max_y:
            continue
        # As long as the pouring water does not hit clay or standing water, keep pouring
        if (x,y+1) not in clay and (x,y+1) not in standing_water:
            pour.append((x,y+1))
        # Else fill if not already filled
        elif (x,y) not in running_water:
            fill.append((x,y))
        running_water.add((x,y))
        
    # Fill until water runs over
    while fill:
        x,y = fill.pop(0)
        # Fill right
        xp = x
        while ((xp,y+1) in clay or (xp,y+1) in standing_water) and (xp+1,y) not in clay:
            xp += 1
        # If over edge
        if (xp,y+1) not in clay and (xp,y+1) not in standing_water:
            pour.append((xp,y))

        # Fill left
        xm = x
        while ((xm,y+1) in clay or (xm,y+1) in standing_water) and (xm-1,y) not in clay:
            xm -= 1
        # If over edge
        if (xm,y+1) not in clay and (xm,y+1) not in standing_water:
            pour.append((xm,y))    

        # Always fill running water
        for xt in range(xm,xp+1):
            running_water.add((xt,y))
        # Fill standing water if not over any edge
        if ((xp,y+1) in clay or (xp,y+1) in standing_water) and ((xm,y+1) in clay or (xm,y+1) in standing_water):
            for xt in range(xm,xp+1):
                standing_water.add((xt,y))
            fill.append((x,y-1))
                
print('Part 1:', len(running_water))
print('Part 2:', len(standing_water))

































