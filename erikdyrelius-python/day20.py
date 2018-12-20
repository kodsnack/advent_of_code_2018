from aocbase import readInput
import re
from collections import deque

inp = readInput()
inp2 = '^ENWWW(NEEE|SSE(EE|N))$'

def parse(s, m, x, y):
    i = 0
    while i < len(s):
        c = s[i]
        if c=='N':
            m[(x, y)]['N'] = (x, y-1)
            m[(x, y-1)] = dict()
            y = y - 1
        elif c=='S':
            m[(x, y)]['S'] = (x, y+1)
            m[(x, y+1)] = dict()
            y = y + 1
        elif c=='W':
            m[(x, y)]['W'] = (x-1, y)
            m[(x-1, y)] = dict()
            x = x - 1
        elif c=='E':
            m[(x, y)]['E'] = (x+1, y)
            m[(x+1, y)] = dict()
            x = x + 1
        elif c == '^':
            m[x, y] = dict()
            while s[i] != '$':
                i += parse(s[i+1:], m, x, y)
        elif c == '(':
            while s[i] != ')':
                i += parse(s[i+1:], m, x, y)
        elif c == '|':
            return i+1
        elif c in ')$':
            return i+1
        i += 1
    return i

def findFurthest(m, x, y):
    vs = dict()
    vs[(x,y)] = 0
    q = [(x, y)]
    while len(q)>0:
        r = q[0]
        q = q[1:]
        for d, (x1, y1) in m[(r[0], r[1])].items():
            if (x1, y1) not in vs:
                vs[x1, y1] = vs[(r[0], r[1])]+1
                q.append((x1, y1))
    sm = 0
    for i in vs.values():
        if i>=1000:
            sm+=1
    return max(vs.values()), sm

m = dict()
parse(inp, m, 0, 0)
print(findFurthest(m, 0, 0))


print("Solution to day 20 part 1:",20.1)
print("Solution to day 20 part 2:",20.2)
