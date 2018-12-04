import re

nrs = {}
coordinates = {}
sum = 0
highx = 0
highy = 0

file = open('input.txt', 'r')
input = file.readlines()
for lines in input:
    row = re.sub('\s+', ' ', lines)
    a = row.split('#')
    b = a[1].split('@')
    c = b[1].split(',')
    d = c[1].split(':')
    e = d[1].split('x')
    nr = int(b[0].strip(' '))
    nrs[nr] = 0
    x = int(c[0].strip(' '))
    y = int(d[0].strip(' '))
    width = int(e[0].strip(' '))
    height = int(e[1].strip(' '))
    # print(nr, x, y, width, height)
    for x1 in range(x, x+width):
        for y1 in range(y, y+height):
            if not x1 in coordinates:
                coordinates[x1] = {}
                if x1 > highx:
                    highx = x1
            if not y1 in coordinates[x1]:
                coordinates[x1][y1] = []
                if y1 > highy:
                    highy = y1
            coordinates[x1][y1].append(nr)
for x1 in range(0, highx):
    for y1 in range(0, highy):
        if x1 in coordinates:
            if y1 in coordinates[x1]:
                if len(coordinates[x1][y1]) > 1:
                    for ids in coordinates[x1][y1]:
                        nrs[ids] += 1

for keys in nrs:
    if nrs[keys] == 0:
        print(keys)
