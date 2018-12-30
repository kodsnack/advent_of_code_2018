#!/usr/bin/env python3
import re
from matplotlib import pyplot as plt
from operator import add

def plotPoints(pos, sec):
    x, y = zip(*pos)
    plt.plot(x, y, 'ro')
    plt.title('Elapsed time: {} seconds'.format(sec))
    plt.show()

def calcNewPos(pos, vel, sec=1):
    dis = [[i * sec for i in v] for v in vel]
    pos = [list(map(add, p, d)) for p, d in zip(pos, dis)]
    return pos

with open('input.txt') as file:
    pos, vel = map(list, zip(*[re.findall(r'(?<=<).+?(?=>)'.strip(), line) for line in file]))

pos = [[int(s) for s in ss.strip().split(',')] for ss in pos]
vel = [[int(s) for s in ss.strip().split(',')] for ss in vel]
xPos, yPos = zip(*pos)
xSize = max(xPos) - min(xPos)
minXSize = xSize
newPos = pos
sec = 0

while xSize <= minXSize:
    minXSize = xSize
    sec += 1
    prevPos = newPos
    newPos = calcNewPos(pos, vel, sec)
    xPos, yPos = zip(*newPos)
    xSize = max(xPos) - min(xPos)

print('Elapsed time: {} seconds'.format(sec - 1))
plotPoints(prevPos, sec - 1)
