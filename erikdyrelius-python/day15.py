from aocbase import readInput
import re

inp = readInput()
inp2 = '''#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######'''
inp3 = '''#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######'''

gg = dict()
ee = dict()
ww = set()
xmx, ymx = 0, 0

for y, line in enumerate(inp.splitlines()):
    ymx = max(y, ymx)
    for x, c in enumerate(line):
        xmx = max(xmx, x)
        if c == 'G':
            gg[(x,y)] = 200
        if c == 'E':
            ee[(x,y)] = 200
        if c == '#':
            ww.add((x,y))
xmx += 1
ymx += 1

delta = [(0,-1), (-1,0), (1,0), (0, 1)]
def calcDistMap(x, y, block):
    mp = dict()
    mp[(x,y)] = []
    q = [(x,y)]
    while len(q) > 0:
        x1, y1 = q[0]
        q = q[1:]
        for dlt in delta:
            x2, y2 = x1+dlt[0], y1+dlt[1]
            if (x2,y2) not in block and (x2, y2) not in mp:
                mp[x2, y2] = [(x2, y2)] + mp[x1,y1]
                q.append((x2, y2))
    return mp

def findMove(xo, yo, xmx, ymx, dest, block):
    dm = calcDistMap(xo, yo, block)
    if len(dm) == 0:
        return (xo,yo)
    best = None
    for x, y in dest.keys():
        for dlt in delta:
            x1, y1 = x+dlt[0], y+dlt[1]
            if (x1, y1) == (xo, yo):
                return (xo, yo)
            if (x1, y1) not in dm:
                continue
            if not best or len(best) >= len(dm[x1, y1]):
                if best and len(best) == len(dm[x1, y1]):
                    if best[0][1] < dm[x1, y1][0][1]:
                        continue
                    if best[0][1] == dm[x1, y1][0][1]:
                        if best[0][0] < dm[x1, y1][0][0]:
                            continue
                best = dm[x1, y1]
    if best:
        return best[-1]
    return (xo, yo)

def target(x, y, targets):
    minhp = 300
    tx, ty = x, y
    for dlt in delta:
        x1, y1 = x+dlt[0], y+dlt[1]
        if (x1, y1) in targets and targets[x1, y1] < minhp:
            minhp = targets[x1, y1]
            tx, ty = x1, y1
    return tx, ty

def move(ww, ee, gg, xmx, ymx, ap):
    theKeys = set(ee.keys())| set(gg.keys())
    for y in range(ymx):
        for x in range(xmx):
            if (x,y) not in theKeys:
                continue
            blocks = ww| set(ee.keys())| set(gg.keys())
            if (x,y) in ee:
                if len(gg)==0:
                    return False
                x1, y1 = findMove(x, y, xmx, ymx, gg, blocks)
                if (x1, y1) != (x, y):
                    ee[(x1, y1)] = ee[(x, y)]
                    del ee[(x, y)]
                xt, yt = target(x1, y1, gg)
                if (xt, yt) != (x1, y1):
                    gg[xt, yt] -= ap
                    if gg[xt, yt] <= 0:
                        del gg[xt, yt]
            elif (x,y) in gg:
                if len(ee)==0:
                    return False
                x1, y1 = findMove(x, y, xmx, ymx, ee, blocks)
                if (x1, y1) != (x, y):
                    gg[(x1, y1)] = gg[(x, y)]
                    del gg[(x, y)]
                xt, yt = target(x1, y1, ee)
                if (xt, yt) != (x1, y1):
                    ee[xt, yt] -= 3
                    if ee[xt, yt] <= 0:
                        del ee[xt, yt]
    return True

def prnt(ww, ee, gg, xmx, ymx, i, ap):
    print("After:", i, "ap=", ap)
    for y in range(ymx):
        for x in range(xmx):
            if (x,y) in ww:
                print("#", end='')
            elif (x,y) in gg:
                print("G", end='')
            elif (x,y) in ee:
                print("E", end='')
            else:
                print(".", end='')
        print()
    print()
    print("Elves:", ee)
    print("Goblins:", gg)

ap = 3
wb, eb, gb = set(ww), dict(ee), dict(gg)
while True:
    i=0
    ww, ee, gg = set(wb), dict(eb), dict(gb)
    while True:
        complete = move(ww, ee, gg, xmx, ymx, ap)
        if len(ee) < len(eb):
            ap += 1
            print("Dead elf")
            prnt(ww, ee, gg, xmx, ymx, i, ap)
            break
        if complete:
            i += 1
        if len(ee)==0:
            break
        if len(gg)==0:
            print(sum(ee.values()), i+1, sum(ee.values())*i)
            break
    if len(ee)==len(eb):
        break

print("Solution to day 15 part 1:",15.1)
print("Solution to day 15 part 2:",15.2)
