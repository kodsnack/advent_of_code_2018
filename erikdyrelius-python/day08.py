from aocbase import readInput
import re

inp = readInput()
#inp = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

s = list(map(int, inp.strip().split()))

def buildTree(s):
    d = dict()
    d["nc"] = s[0]
    d["nm"] = s[1]
    d["c"] = []
    s = s[2:]
    for i in range(d["nc"]):
        dc, s = buildTree(s)
        d["c"].append(dc)
    if d["nm"] == 0:
        d["m"] == []
    else:
        d["m"] = s[:d["nm"]]
        s = s[d["nm"]:]
    return d, s

def calcMeta(d):
    sm = sum(d["m"])
    for i in d["c"]:
        sm += calcMeta(i)
    return sm

def calcValue(d):
    if d["nc"] == 0:
        return sum(d["m"])
    else:
        sm = 0
        for i in d["m"]:
            if 0 < i <= d["nc"]:
                sm += calcValue(d["c"][i-1])
        return sm

t, _ = buildTree(s)

print("Solution to day 8 part 1:", calcMeta(t))

print("Solution to day 8 part 2:", calcValue(t))
