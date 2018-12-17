from aocbase import readInput
import re

inp = readInput()
inp2='''x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504'''
p = re.compile(r"(.)=(\d+), (.)=(\d+)\.\.(\d+)")

def parse(s):
    l = []
    for line in s.splitlines():
        m = p.match(line)
        l.append(m.groups())
    return l

def createGround(l):
    d = dict()
    mnx, mny, mxx, mxy = 500, 0, 500, 0
    for patch in l:
        if patch[0]=='x':
            x = int(patch[1])
            for y in range(int(patch[3]),int(patch[4])+1):
                d[(x,y)] = '#'
                mnx = min(x, mnx)
                mny = min(y, mny)
                mxx = max(x, mxx)
                mxy = max(y, mxy)
        if patch[0]=='y':
            y = int(patch[1])
            for x in range(int(patch[3]),int(patch[4])+1):
                d[(x,y)] = '#'
                mnx = min(x, mnx)
                mny = min(y, mny)
                mxx = max(x, mxx)
                mxy = max(y, mxy)
    d[(500,0)] = '+'
    return d, mnx-1, mny, mxx+1, mxy

def drawGround(mnx, mny, mxx, mxy, d):
    for y in range(mny,mxy+1):
        if y%50 == 0:
            for dig in range(3,-1,-1):
                print("     ",end='')
                for x in range(mnx, mxx+1):
                    print(x//10**dig % 10, end='')
                print()
        print("{:04d}".format(y), end=' ')
        for x in range(mnx, mxx+1):
            if (x,y) in d:
                print(d[(x,y)], end='')
            else:
                print('.',end='')
        print()

def spread(mnx, mny, mxx, mxy, d):
    i = 0
    changed = True
    lastd = d
    while changed:
        changed = False
        newd = dict()
        for (x, y), c in lastd.items():
            if c == '+':
                if (x,y+1) not in d and mnx<=x<=mxx and mny<y+1<=mxy:
                    newd[(x,y+1)] = '|'
                    changed = True
            if c == '|':
                if (x,y+1) not in d:
                    if mnx<=x<=mxx and mny<y+1<=mxy:
                        newd[(x,y+1)] = '|'
                        changed = True
                elif (x,y+1) in d and d[(x,y+1)] in '~#':
                    if (x-1,y) not in d:
                        if mnx<=x-1<=mxx and mny<y<=mxy:
                            newd[(x-1,y)] = '|'
                            changed = True
                    if (x+1,y) not in d:
                        if mnx<=x+1<=mxx and mny<y<=mxy:
                            newd[(x+1,y)] = '|'
                            changed = True
        d.update(newd)
        if len(newd) > 0:
            starty = min(min(map(lambda x:x[1], newd.keys())),min(map(lambda x:x[1], lastd.keys())))-1
            endy = max(max(map(lambda x:x[1], newd.keys())), max(map(lambda x:x[1], lastd.keys())))+2
            for y in range(starty, endy):
                start = False
                startX = True
                for x in range(mnx, mxx+1):
                    if ((x,y) in d and d[(x,y)] == '|' and 
                        (x-1, y) in d and d[(x-1, y)] == '#' and
                        (x, y+1) in d and d[(x, y+1)] in '#~'):
                        start = True
                        startX = x
                    elif (start and
                        (x,y) in d and d[(x,y)] == '|' and 
                        (x, y+1) in d and d[(x, y+1)] in '#~'):
                        pass
                    elif (start and
                        (x,y) in d and d[(x,y)] == '#'):
                        for xx in range(startX, x):
                            d[(xx,y)] = '~'
                            if (xx,y-1) in d and d[(xx,y-1)]=='|':
                                newd[(xx,y-1)] = d[(xx,y-1)]
                            changed = True
                        start = False
                    else:
                        start = False
        lastd = newd
        if i%1000==0:
            print(i, list(d.values()).count('|')+list(d.values()).count('~'))
        i+=1
    drawGround(mnx, mny, mxx, mxy, d)
    return list(d.values()).count('|')+list(d.values()).count('~'), list(d.values()).count('~')

l = parse(inp)
d, mnx, mny, mxx, mxy = createGround(l)
#drawGround(mnx, mny, mxx, mxy, d)
print(spread(mnx, mny, mxx, mxy, d))
#print("Solution to day 17 part 1:",17.1)
#print("Solution to day 17 part 2:",17.2)
