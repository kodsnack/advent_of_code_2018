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
    return list(s.splitlines())

def gen(l):
    l2 = list()
    szx, szy = len(l[0]), len(l)
    sm = {'|':0, '#':0, '.':0}
    for y in range(szy):
        l3 = list()
        for x in range(szx):
            sm['|'] = sm['.'] = sm['#'] = 0
            for dx in range(-1, 2):
                for dy in range(-1, 2):
                    if dx==0 and dy==0:
                        continue
                    x2, y2 = x+dx, y+dy
                    if 0>x2 or x2>=len(l[y]):
                        continue
                    if 0>y2 or y2>=len(l):
                        continue
                    sm[l[y2][x2]] += 1
            c = l[y][x]
            if c=='.':
                if sm['|']>=3:
                    l3.append('|')
                else:
                    l3.append('.')
            elif c=='|':
                if sm['#']>=3:
                    l3.append('#')
                else:
                    l3.append('|')
            elif c=='#':
                if sm['#']>=1 and sm["|"]>=1:
                    l3.append('#')
                else:
                    l3.append('.')
            else:
                print("Error")
        l2.append(''.join(l3))
    return l2

def prnt(l):
    print('\n'.join(line))

def shortTimeValue(tm, l):
    for i in range(tm):
        l = gen(l)
    s = ''.join(l)
    return s.count("#")*s.count("|")

def longTimeValue(tm, l):
    cache = [0]
    i = 1
    while True:
        l = gen(l)
        s = ''.join(l)
        rv = s.count("#")*s.count("|")
        if i==tm:
            return rv
        if cache.count(rv) >= 2:
            first = cache.index(rv)
            second = cache.index(rv, first+1)
            if tm%(second-first) == second%(second-first):
                return rv
        cache.append(rv)
        i += 1

l = parse(inp)
print("Solution to day 18 part 1:",shortTimeValue(10, l))
print("Solution to day 18 part 2:",longTimeValue(1000000000, l))
