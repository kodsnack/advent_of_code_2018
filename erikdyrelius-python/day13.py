inp = open("day13.txt").read()
inp2='''/->-\        
|   |  /----\\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   
'''
inp3='''/>-<\  
|   |  
| /<+-\\
| | | v
\>+</ |
  |   ^
  \<->/   
'''

def parseInp(inp):
    y = 0
    t = dict()
    d = list()
    for line in inp.splitlines():
        x = 0
        for c in line:
            if c in '-/\\|+':
                t[(x,y)] = c
            elif c == '^':
                d.append((y, x, 0, 0))
                t[(x,y)] = '|'
            elif c == 'v':
                d.append((y, x, 2, 0))
                t[(x,y)] = '|'
            elif c == '<':
                d.append((y, x, 3, 0))
                t[(x,y)] = '-'
            elif c == '>':
                d.append((y, x, 1, 0))
                t[(x,y)] = '-'
            x += 1
        y = y + 1
    return t, d

def collision(d):
    s = set()
    for y,x,di,st in d:
        if (x,y) in s:
            return (x,y)
        s.add((x, y))
    return (-1,-1)

def removeColl(d, ci):
    for i in range(len(d)-1):
        for j in range(i+1, len(d)):
            if d[i][:2] == d[j][:2]:
                d[j:j+1] = []
                d[i:i+1] = []
                if i < ci:
                    ci -= 2
                else:
                    ci -= 1
                return d, ci

def step(t, d, rc=False):
    i = 0
    while i < len(d):
        y, x, di, st = d[i][0], d[i][1], d[i][2], d[i][3]
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
            elif st == 1:
                pass
            elif st == 2:
                di2 = (di + 1)%4
            st2 = (st+1)%3
        d[i] = (y2, x2, di2, st2)
        if collision(d) != (-1,-1):
            if rc:
                d, i=removeColl(d, i)
            else:
                break
        i += 1
    return sorted(d)

t, d = parseInp(inp)
while collision(d) == (-1, -1):
    d = step(t, d)
col = collision(d)
print("Solution to day 12 part 1: {},{}".format(col[0], col[1]))

t, d = parseInp(inp)
while len(d)>1:
    d = step(t, d, True)
print("Solution to day 12 part 2: {},{}".format(d[0][1], d[0][0]))
