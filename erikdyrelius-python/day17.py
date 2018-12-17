from aocbase import readInput
import re
from collections import deque

inp = readInput()
inp2='''x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504'''
p = re.compile(r"(.)=(\d+), (.)=(\d+)\.\.(\d+)")

def parse(s):
    l = []
    for line in s.splitlines():
        m = p.match(line)
        l.append(m.groups())
    return l

def createGround(l):
    d = dict()
    mnx, mny, mxx, mxy = 500, 0, 500, 0
    for patch in l:
        if patch[0]=='x':
            x = int(patch[1])
            for y in range(int(patch[3]),int(patch[4])+1):
                d[(x,y)] = '#'
                mnx = min(x, mnx)
                mny = min(y, mny)
                mxx = max(x, mxx)
                mxy = max(y, mxy)
        if patch[0]=='y':
            y = int(patch[1])
            for x in range(int(patch[3]),int(patch[4])+1):
                d[(x,y)] = '#'
                mnx = min(x, mnx)
                mny = min(y, mny)
                mxx = max(x, mxx)
                mxy = max(y, mxy)
    d[(500,0)] = '+'
    return d, mnx-1, mny, mxx+1, mxy

def drawGround(mnx, mny, mxx, mxy, d):
    for y in range(mny,mxy+1):
        if y%50 == 0:
            for dig in range(3,-1,-1):
                print("     ",end='')
                for x in range(mnx, mxx+1):
                    print(x//10**dig % 10, end='')
                print()
        print("{:04d}".format(y), end=' ')
        for x in range(mnx, mxx+1):
            if (x,y) in d:
                print(d[(x,y)], end='')
            else:
                print('.',end='')
        print()

def calculateScore(d, countMoving):
    mny, mxy = 10000, 0
    for (x, y), c in d.items():
        if c == '#':
            mny = min(y, mny)
            mxy = max(y, mxy)
    sm = 0
    for (x, y), c in d.items():
        if y < mny or y > mxy:
            continue
        if c == '~' or (countMoving and c == '|'):
            sm += 1
    return sm

def spreadStill(d, x, y, q):
    while (x,y) in d and d[(x, y)] == '|' and (x, y+1) in d and d[(x, y+1)] in '#~':
        x -= 1
    if (x, y) not in d or d[(x, y)] != '#' or (x, y+1) not in d or d[(x, y+1)] not in '#~':
        return
    xx = x + 1
    while (xx, y) in d and d[(xx, y)] == '|' and (xx, y+1) in d and d[(xx, y+1)] in '#~':
        xx += 1
    if (xx, y) not in d or d[(xx, y)] != '#' or (xx, y+1) not in d or d[(xx, y+1)] not in '#~':
        return
    for i in range(x+1, xx):
        d[(i, y)] = '~'
        if (i, y-1) in d and d[(i, y-1)] == '|':
            q.append((i, y-1))

def spread(mnx, mny, mxx, mxy, d):
    changed = True
    lastd = d
    q = deque()
    q.append((500, 0))
    while len(q) > 0:
        x, y = q.popleft()
        c = d[(x, y)]
        if c == '+':
            if (x,y+1) not in d and y+1 <= mxy:
                d[(x, y+1)] = '|'
                q.append((x, y+1))
        if c == '|':
            if (x,y+1) not in d:
                if y+1 <= mxy:
                    d[(x,y+1)] = '|'
                    q.append((x, y+1))
            elif (x,y+1) in d and d[(x,y+1)] in '~#':
                if (x-1,y) not in d:
                    d[(x-1,y)] = '|'
                    q.append((x-1, y))
                else:
                    spreadStill(d, x, y, q)
                if (x+1,y) not in d:
                    d[(x+1,y)] = '|'
                    q.append((x+1, y))
                else:
                    spreadStill(d, x, y, q)

l = parse(inp)
d, mnx, mny, mxx, mxy = createGround(l)
spread(mnx, mny, mxx, mxy, d)
print("Solution to day 17 part 1:",calculateScore(d, True))
print("Solution to day 17 part 2:",calculateScore(d, False))
