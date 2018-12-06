from aocbase import readInput
import re
from functools import reduce

inp = readInput()
p = re.compile(r"^(\d+), (\d+)$")

def lineParse(s, f=lambda x:x):
    m = p.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x):
    return list(map(lambda x, fx=ff:f(x, fx), inp.splitlines()))

s = fileParse(inp, ff=int)
mnx = reduce(min, (map(lambda x: x[0], s)))
mxx = reduce(max, (map(lambda x: x[0], s)))
mny = reduce(min, (map(lambda y: y[1], s)))
mxy = reduce(max, (map(lambda y: y[1], s)))
print(mnx, mxx, mny, mxy)
d = dict()
c = 1
for i in s:
    d[i] = c
    c = c + 1
dd = ((1, 0), (0, 1), (-1, 0), (0, -1))
changed = True
while changed:
    changed = False
    d2 = dict()
    for x in range(mnx, mxx+1):
        for y in range(mny, mxy+1):
            if (x, y) not in d:
                ss = set()
                for dx, dy in dd:
                    if (x+dx,y+dy) in d:
                        ss.add(d[(x+dx, y+dy)])
                if len(ss) == 1:
                    d2[(x, y)] = list(ss)[0]
                    changed = True
    for key in d2:
        d[key] = d2[key]
d3 = dict()
s1 = set()
s2 = set()
for key in d:
    d3[d[key]] = d3.get(d[key], 0) + 1
    s1.add(d[key])
    if key[0] in (mnx, mxx) or key[1] in (mny, mxy):
        s2.add(d[key])
print(s1, s2, s1-s2)
mna = 0
for i in s1-s2:
    if d3[i] > mna:
        mna = d3[i]

d4 = dict()
for x in range(mnx, mxx+1):
    for y in range(mny, mxy+1):
        for i in s:
            d4[(x, y)] = d4.get((x, y), 0) + abs(x-i[0]) + abs(y-i[1])
sm = 0
for i in d4:
    if d4[i] < 10000:
        sm += 1
print("Solution to day 6 part 1:", mna)

print("Solution to day 6 part 2:", sm)
