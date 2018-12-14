from aocbase import readInput
import re

inp = readInput()
# inp = '''Step C must be finished before step A can begin.
# Step C must be finished before step F can begin.
# Step A must be finished before step B can begin.
# Step A must be finished before step D can begin.
# Step B must be finished before step E can begin.
# Step D must be finished before step E can begin.
# Step F must be finished before step E can begin.'''
p = re.compile(r"^Step (.) must be finished before step (.) can begin\.$")

def lineParse(s, f=lambda x:x):
    m = p.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x):
    return list(map(lambda x, fx=ff:f(x, fx), inp.splitlines()))

s = fileParse(inp, ff=str)
d = dict()
s2 = set()
for c1, c2 in s:
    if c2 not in d:
        d[c2] = set()
    if c1 not in d:
        d[c1] = set()
    s2.add(c1)
    s2.add(c2)
    d[c2].add(c1)
s3 = set()
s4 = set(s2)
print(d)
sol = ""
while s2 != s3:
    order = sorted(list(s2))
    for c in order:
        ready = True
        if c in s3:
            continue
        for cc in d[c]:
            if cc not in s3:
                ready = False
                break
        if ready:
            s3.add(c)
            sol = sol + c
            break
print("Solution to day 7 part 1:", sol)
s2 = set(s4)
wd = dict()
for c in s2:
    wd[c] = ord(c)-ord('A')+61
s3 = set()
sol = ""
i = 0
while s2 != s3:
    order = sorted(list(s2))
    w = 5
    wo = ""
    for c in order:
        ready = True
        if c in s3:
            continue
        for cc in d[c]:
            if cc not in s3:
                ready = False
                break
        if ready:
            wo = wo + c
            w = w - 1
            if w == 0:
                break
    for c in wo:
        wd[c] -= 1
        if wd[c] == 0:
            sol += c
            s3.add(c)
            print(i, c)
    i = i + 1

print("Solution to day 7 part 2:", i)
