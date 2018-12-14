from aocbase import readInput

def intGen(inp):
    for i in inp.strip().split():
        yield int(i)

def buildTree(it):
    d = {"nc":it.__next__(), "nm":it.__next__(), "m":[]}
    d["c"] = [buildTree(it) for i in range(d["nc"])]
    d["m"] = [it.__next__() for i in range(d["nm"])]
    return d

def calcMeta(d):
    return sum(d["m"]) + sum(map(calcMeta, d["c"]))

def calcValue(d):
    if d["nc"] == 0:
        return sum(d["m"])
    else:
        return sum([calcValue(d["c"][i-1]) for i in d["m"] if 0 < i <= d["nc"]])

inp = readInput()
t = buildTree(intGen(inp))
print("Solution to day 8 part 1:", calcMeta(t))
print("Solution to day 8 part 2:", calcValue(t))