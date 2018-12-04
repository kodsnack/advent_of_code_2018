from aocbase import readInput
import re

inp = readInput()
# [1518-04-21 00:57] wakes up
p = re.compile(r"^\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)$")

def linePar(s, f=lambda x:x):
    m = p.match(s)
    if m==None:
        print(s)
    return tuple(map(f, m.groups()))

def filePar(inp, f=linePar, ff=lambda x:x):
    return list(map(lambda x, fx=ff:f(x, fx), inp.splitlines()))

s = filePar(inp)
s.sort()
d = dict()
gi = "None"
gs = 0
#print(s)
for i in s:
    if i[5][0] == 'G':
        gi = i[5].split()[1][1:]
    if i[5][0] == 'f':
        gs = int(i[4])
        #print(gs)
    if i[5][0] == 'w':
        d[gi] = d.get(gi, 0) + int(i[4]) - gs
mt = 0
mg = 0
for i in d:
    if d[i] > mt:
        mt = d[i]
        mg = i
#print(d)
#print(mt, mg)
d2 = [0]*59
on = False
mxt, mxm = 0, 0
for i in s:
    if i[5][0] == 'G':
        if mg == i[5].split()[1][1:]:
            on = True
        else:
            on = False
    if on:
        pass
        #print(i)
    if i[5][0] == 'f':
        gs = int(i[4])
        #print(gs)
    if i[5][0] == 'w':
        if on == True:
            for j in range(gs, int(i[4])):
                d2[j] += 1
                if d2[j] > mxt:
                    mxt = d2[j]
                    mxm = j
#print(d2)
#print(mxt, mxm, mg, int(mg)*mxm)
mxt, mxm, mxg = 0, 0, ""
d3 = dict()
for i in s:
    if i[5][0] == 'G':
        gg = i[5].split()[1][1:]
    if i[5][0] == 'f':
        gs = int(i[4])
    if i[5][0] == 'w':
        for j in range(gs, int(i[4])):
            if gg not in d3:
                d3[gg] = [0]*60
            d3[gg][j] += 1
            if d3[gg][j] > mxt:
                mxt = d3[gg][j]
                mxm = j
                mxg = gg
                print(mxg, mxt, mxm, mxm*int(mxg))
print("Solution to day 4 part 1:", int(mg)*mxm)
print("Solution to day 4 part 2:", 4.2)
