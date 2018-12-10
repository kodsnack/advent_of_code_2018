from aocbase import readInput
import re

inp = readInput()
p = re.compile(r"-?\d+")

def lineParse(s, f=lambda x:x):
    return tuple(map(f, p.findall(s)))

def fileParse(inp, f=lineParse, ff=lambda x:x):
    return list(map(lambda x, fx=ff:f(x, fx), inp.splitlines()))

def findMsg(p):
    t = 8000
    while True:
        p1 = list(map(lambda a,t1=t:(a[0]+t1*a[2],a[1]+t1*a[3]), p))
        mnx = min(map(lambda x:x[0], p1))
        mxx = max(map(lambda x:x[0], p1))
        mny = min(map(lambda x:x[1], p1))
        mxy = max(map(lambda x:x[1], p1))
        if (mxx-mnx+mxy-mny < 80):
            print("Solution to day 10 part 1:")
            for y in range(mny, mxy+1):
                for x in range(mnx, mxx+1):
                    if (x,y) in p1:
                        print("#", end='')
                    else:
                        print(".", end='')
                print()
            print("Solution to day 10 part 2:",t)
            break
        t += 1
            
points = fileParse(inp, ff=int)
findMsg(points)
