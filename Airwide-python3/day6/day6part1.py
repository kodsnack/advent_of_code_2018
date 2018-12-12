#!/usr/bin/env python3
import string
import numpy as np
import itertools

def iter_all_strings():
    for size in itertools.count(1):
        for s in itertools.product(string.ascii_lowercase, repeat=size):
            yield "".join(s)

with open("input.txt") as input_file:
    coords = [tuple(map(int, line.replace(' ','').strip().split(','))) for line in input_file]
chars = list(itertools.islice(iter_all_strings(), len(coords)))

# Part 1
xMin = min(coords, key=lambda item:item[0])[0]
xMax = max(coords, key=lambda item:item[0])[0]
yMin = min(coords, key=lambda item:item[1])[1]
yMax = max(coords, key=lambda item:item[1])[1]

grid = np.full((yMax + 1, xMax + 1),' ',dtype='U10')

for x in range(xMin,xMax + 1):
    for y in range(yMin,yMax + 1):
        distList = []
        for i in range(len(coords)):
            dist = abs(x - coords[i][0]) + abs(y - coords[i][1])
            distList.append(dist)
        minDist = min(distList)
        if distList.count(minDist) > 1:
            char = '.'
        else:
            char = chars[distList.index(minDist)]
        # if minDist == 0:
        #     char = char.upper()
        grid[y, x] = char

grid = grid[xMin:,yMin:]            
unique, counts = np.unique(grid, return_counts=True)
countsDict = dict(zip(unique, counts))
perimeterChars = np.append(grid[:,[0, -1]], grid[[0, -1]].T)

infiniteChars = np.unique(perimeterChars, axis=0)
for each in infiniteChars:
    countsDict.pop(each, None)

print(countsDict)
print("Day 6 part 1 result: {}".format(max(countsDict.values())))

# Part 2
