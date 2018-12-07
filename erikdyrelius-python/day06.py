from aocbase import readInput
import re
from functools import reduce
import timeit

inp = readInput()
p = re.compile(r"^(\d+), (\d+)$")

def lineParse(s, f=lambda x:x):
    m = p.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x):
    return list(map(lambda x, fx=ff:f(x, fx), inp.splitlines()))

def findBoundingBox(s):
    return (reduce(min, (map(lambda x: x[0], s))), 
            reduce(min, (map(lambda y: y[1], s))),
            reduce(max, (map(lambda x: x[0], s))),
            reduce(max, (map(lambda y: y[1], s))))

def calculateMap(s, mnx, mny, mxx, mxy):
    d = dict()
    c = 1
    for i in s:
        d[i] = c
        c = c + 1
    deltas = ((1, 0), (0, 1), (-1, 0), (0, -1))
    changed = True
    while changed:
        changed = False
        d2 = dict()
        for x in range(mnx, mxx+1):
            for y in range(mny, mxy+1):
                if (x, y) in d: continue
                ss = set()
                for dx, dy in deltas:
                    p = (x+dx,y+dy) 
                    if p in d:
                        ss.add(d[p])
                if len(ss) == 1:
                    d2[(x, y)] = ss.pop()
                    changed = True
        d.update(d2)
    return d

def calcFinites(cnm):
    d3 = dict()
    s1 = set()
    s2 = set()
    for key, val in cnm.items():
        d3[val] = d3.get(val, 0) + 1
        s1.add(val)
        if key[0] in (mnx, mxx) or key[1] in (mny, mxy):
            s2.add(val)
    return d3, s1-s2

def calculateDistances(s, mnx, mny, mxx, mxy):
    dist = dict()
    for x in range(mnx, mxx+1):
        for y in range(mny, mxy+1):
            for i in s:
                dist[(x, y)] = dist.get((x, y), 0) + abs(x-i[0]) + abs(y-i[1])
    return dist

s = fileParse(inp, ff=int)

mnx, mny, mxx, mxy = findBoundingBox(s)
closenessMap = calculateMap(s, mnx, mny, mxx, mxy)
finiteSums, finites = calcFinites(closenessMap)
mna = max([finiteSums[i] for i in finites])
print("Solution to day 6 part 1:", mna)

dist = calculateDistances(s, mnx, mny, mxx, mxy)
sm = sum(map(lambda x:1 if x < 10000 else 0, dist.values()))

print("Solution to day 6 part 2:", sm)
