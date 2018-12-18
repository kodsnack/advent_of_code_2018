from aocbase import readInput
import re

inp = readInput()
inp2 = '''.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.'''

def parse(s):
    l = []
    for line in s.splitlines():
        l.append(list(line))
    return l

def gen(l):
    l2 = list()
    szx, szy = len(l[0]), len(l)
    sm = {'|':0, '#':0, '.':0}
    for y in range(szy):
        l2.append([])
        for x in range(szx):
            for dx in range(-1, 2):
                sm['|'] = sm['.'] = sm['#'] = 0
                for dy in range(-1, 2):
                    if dx==0 and dy==0:
                        continue
                    x2, y2 = x+dx, y+dy
                    if 0>x2 or x2>=len(l[y]):
                        continue
                    if 0>y2 or y2>=len(l):
                        continue
                    c = l[y2][x2]
                    sm[c] += 1
            c = l[y][x]
            if c=='.':
                if sm['|']>=3:
                    l2[y].append('|')
                else:
                    l2[y].append('.')
            elif c=='|':
                if sm['#']>=3:
                    l2[y].append('#')
                else:
                    l2[y].append('|')
            elif c=='#':
                if sm['#']>=1 and sm["|"]>=1:
                    l2[y].append('#')
                else:
                    l2[y].append('.')
            else:
                print("Error")
    return l2

def prnt(l):
    for line in l:
        print(''.join(line))

l = parse(inp)
cache = [0]
for i in range(1, 2000):
    l = gen(l)
    #prnt(l)
    #print()
    s = ''.join(map(lambda x:''.join(x),l))
    #print(s)
    rv = s.count("#")*s.count("|")
    if cache.count(rv) >= 2:
        first = cache.index(rv)
        second = cache.index(rv, first+1)
        if 1000000000%(second-first) == second%(second-first):
            print(rv)
            break
    cache.append(rv)

print("Solution to day 18 part 1:",18.1)
print("Solution to day 18 part 2:",18.2)
