from aocbase import readInput
import re

inp = readInput()
inp='''/->-\        
|   |  /----\\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   
'''

y = 0
t = dict()
d = list()
for line in inp.splitlines():
    x = 0
    for c in line:
        if c in '-/\\|+':
            t[(x,y)] = c
        if c == '^':
            d.append((x, y, 0, 0))
            t[(x,y)] = '|'
        if c == 'v':
            d.append((x, y, 2, 0))
            t[(x,y)] = '|'
        if c == '<':
            d.append((x, y, 3, 0))
            t[(x,y)] = '-'
        if c == '>':
            d.append((x, y, 1, 0))
            t[(x,y)] = '-'
        x += 1
    y = y + 1

def collision(d):
    s = set()
    for x,y,di,st in d:
        s.add((x, y))
    if len(s)!= len(d):
        return (x,y)
    else:
        return (-1,-1)

def step(t, d):
    for i in range(len(d)):
        x, y, di, st = d[i][0], d[i][1], d[i][2], d[i][3]
        x2, y2, di2, st2 = x,y,di,st
        if di == 0:
            y2 -= 1
            if t[(x2, y2)] == '\\':
                di2 = 3
            if t[(x2, y2)] == '/':
                di2 = 1
        elif di == 1:
            x2 += 1
            if t[(x2, y2)] == '\\':
                di2 = 2
            if t[(x2, y2)] == '/':
                di2 = 0
        elif di == 2:
            y2 += 1
            if t[(x2, y2)] == '\\':
                di2 = 1
            if t[(x2, y2)] == '/':
                di2 = 3
        else:
            x2 -= 1
            if t[(x2, y2)] == '\\':
                di2 = 0
            if t[(x2, y2)] == '/':
                di2 = 2
        if t[(x2, y2)] == '+':
            if st == 0:
                di2 = (di + 3)%4
            if st == 1:
                pass
            if st == 0:
                di2 = (di + 1)%4
            st2 = (st+1)%3
        d[i] = (x2, y2, di2, st2)
        if collision(d) != (-1,-1):
            break
    return d

while collision(d) == (-1, -1):
    d = step(t, d)

print(collision(d))

print("Solution to day 12 part 1: {}".format(12.1))
print("Solution to day 12 part 2: {}".format('12.2'))
