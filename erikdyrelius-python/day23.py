from aocbase import readInput
import re
from collections import deque

inp = readInput()
inp1='''pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1'''
inp2='''pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5'''
p = re.compile(r"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)")

def parse(s):
    bots = []
    for line in s.splitlines():
        m = p.match(line)
        if m:
            bots.append((int(m.group(4)), int(m.group(1)), int(m.group(2)), int(m.group(3))))
        else:
            print("Error:", line)
    return bots

def inRangeOf(r, bots):
    rng = r[0]
    x1, y1, z1 = r[1:]
    cnt = 0
    for r2 in bots:
        x2, y2, z2 = r2[1:]
        sm = abs(x1-x2)+abs(y1-y2)+abs(z1-z2)
        if sm <= rng:
            cnt += 1
    return cnt

def inRange(pos, bots):
    cnt = 0
    x, y, z = pos
    for r2, x2, y2, z2 in bots:
        sm = abs(x-x2)+abs(y-y2)+abs(z-z2)
        if sm <= r2:
            cnt += 1
    return cnt

def nofOverlaps(bot, bots):
    cnt = 0
    r, x, y, z = bot
    for r2, x2, y2, z2 in bots:
        sm = abs(x-x2)+abs(y-y2)+abs(z-z2)
        if sm <= r2+r:
            cnt += 1
    return cnt

def findExtremes(bots):
    xs = [x[1]-x[0] for x in bots] + [x[1]+x[0] for x in bots]
    ys = [x[2]-x[0] for x in bots] + [x[2]+x[0] for x in bots]
    zs = [x[3]-x[0] for x in bots] + [x[3]+x[0] for x in bots]
    return (min(xs), max(xs), min(ys), max(ys), min(zs), max(zs))

def center(bots):
    xs, ys, zs = 0, 0, 0
    for r, x, y, z in bots:
        xs += x
        ys += y
        zs += z
    nofBots = len(bots)
    return xs//nofBots, ys//nofBots, zs//nofBots

def findOptimalPoint(bots):
    xmn, xmx, ymn, ymx, zmn, zmx = findExtremes(bots)
    xmid, ymid, zmid = (xmx+xmn)//2, (ymx+ymn)//2, (zmx+zmn)//2
    rad = (max((xmx-xmn, ymx-ymn, zmx-zmn))+1)//2
    while True:
        newRad = min((rad*2+2)//3, rad-1)
        prospects = ((newRad, xmid+(rad-newRad), ymid, zmid),
                    (newRad, xmid-(rad-newRad), ymid, zmid),
                    (newRad, xmid, ymid+(rad-newRad), zmid),
                    (newRad, xmid, ymid-(rad-newRad), zmid),
                    (newRad, xmid, ymid, zmid+(rad-newRad)),
                    (newRad, xmid, ymid, zmid-(rad-newRad)),
                    (newRad, xmid, ymid, zmid))
        bestC = 0
        bestProspect = None
        for prospect in prospects:
            c = nofOverlaps(prospect, bots)
            if c > bestC:
                bestC = c
                bestProspect = prospect
        rad, xmid, ymid, zmid = bestProspect
        if rad==0:
            return xmid+ymid+zmid

bots = sorted(parse(inp))
print("Solution to day 23 part 1:",inRangeOf(bots[-1], bots))
print("Solution to day 23 part 2:",findOptimalPoint(bots))
