#!/usr/bin/env python3

import numpy as np

def calcRackID(y,x):
    return x + 11

def calcStartPower(y,x):
    return (x + 11) * (y + 1)

def createGrid(gridSerialNo):
    rackID = np.fromfunction(calcRackID, (300, 300), dtype=int)
    power = np.fromfunction(calcStartPower, (300,300), dtype=int)
    power += gridSerialNo
    power *= rackID
    power = power // 100 - power // 1000 *10
    power -= 5
    return power

def getMaxPowerSquare(grid, size=3):
    squares = {}
    for y in range(grid.shape[0] - (size - 1)):
        for x in range(grid.shape[1] - (size - 1)):
            square = grid[y:y+size,x:x+size]
            squarePower = np.sum(square)
            squares[(x+1,y+1)] = squarePower
    return max(zip(squares.values(), squares.keys()))

def getMaxPwrSqrSize(grid):
    sqrSizePwr = {}
    for i in range(grid.shape[0]):
        power, pos = getMaxPowerSquare(grid, i + 1)
        sqrSizePwr[i+1] = power
    return max(zip(sqrSizePwr.values(), sqrSizePwr.keys()))    


gridSerialNo = 5235
grid = createGrid(gridSerialNo)
power, pos = getMaxPowerSquare(grid)
print('Day 11 part 1 answer: Max power is {} at position {}'.format(power, pos))
maxPwr, size = getMaxPwrSqrSize(grid)
power, pos = getMaxPowerSquare(grid, size)
result = '{},{},{}'.format(pos[0], pos[1], size)
print('Day 11 part 2 answer: Max power is {} at position {} with a size of {} = "{}"'.format(pos, power, size, result))



