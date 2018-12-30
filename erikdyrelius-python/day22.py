from aocbase import readInput
from heapq import heapify, heappop, heappush
from sys import setrecursionlimit

setrecursionlimit(5000)
depth=4845
target=(6,770)
start=(0,0)
depth1 = 510
target1 = (10,10)

geologicIndices = dict()
def geologicIndex(x, y):
    if (x, y) in geologicIndices:
        return geologicIndices[x, y]
    if (x, y) in (start, target):
        result = 0
    elif y == 0:
        result = (x*16807)%20183
    elif x == 0:
        result = (y*48271)%20183
    else:
        result = (erosionLevel(x-1, y)*erosionLevel(x, y-1)) % 20183
    geologicIndices[x, y] = result
    return result

def erosionLevel(x, y):
    return (geologicIndex(x, y) + depth) % 20183

regionTypes = dict()
def regionType(x, y):
    if (x,y) not in regionTypes:
        regionTypes[x, y] = erosionLevel(x,y) % 3
    return regionTypes[x, y]

def calcTotalRisk(start, target):
    minx, maxx = min(start[0], target[0]), max(start[0], target[0])
    miny, maxy = min(start[1], target[1]), max(start[1], target[1])
    return sum(regionType(x,y) for x in range(minx, maxx+1) for y in range(miny, maxy+1))

def drawCave(start, target):
    minx, maxx = min(start[0], target[0]), max(start[0], target[0])
    miny, maxy = min(start[1], target[1]), max(start[1], target[1])
    for y in range(miny, maxy+1):
        for x in range(minx, maxx+1):
            print({0:'.',1:'=',2:'|'}[regionType(x,y)], end='')
        print()

deltas = ((0,1,0), (1,0,0), (-1,0,0), (0,-1,0), 
          (0,0,1), (0,0,2))
def calcCost(start, target, lim=False, maxCost=10**9):
    cost = dict()
    q = [(0, target[0], target[1], 1)]
    heapify(q)
    while len(q)>0:
        c1, x1, y1, e1 = heappop(q)
        if (x1, y1, e1) in cost:
            continue
        cost[x1, y1, e1] = c1
        if (x1, y1) == start and e1 == 1:
            return cost
        for delta in deltas:
            x2, y2, e2 = (x1+delta[0], y1+delta[1], (e1+delta[2])%3)
            if lim and x2 > target[0]*2:
                continue
            if x2 < 0 or y2 < 0:
                continue
            if e2 == regionType(x2, y2):
                continue
            if e1 != e2:
                c2 = 7 + c1
            else:
                c2 = 1 + c1
            if c2+x2+y2 < maxCost:
                heappush(q, (c2, x2, y2, e2))
    return cost
print("Solution to day 22 part 1:", calcTotalRisk(start, target))
cost = calcCost(start, target, lim=True)
cost = calcCost(start, target, lim=False, maxCost=cost[0, 0, 1])
print("Solution to day 22 part 2:", cost[0, 0, 1])
