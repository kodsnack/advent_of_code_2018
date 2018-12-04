from aocbase import readInput
import re

inp = readInput()

p = re.compile(r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

def linePar(s):
    m = p.match(s)
    return tuple(map(int, m.groups()))

claims = map(linePar, inp.splitlines())
land = dict()
doublec = set()
claimed = set()
allc = set()
for claim in claims:
    allc.add(claim[0])
    for x in range(claim[1], claim[1]+claim[3]):
        for y in range(claim[2], claim[2]+claim[4]):
            if (x, y) in land:
                doublec.add((x,y))
                claimed.add(claim[0])
                claimed.add(land[(x,y)])
            else:
                land[(x, y)] = claim[0]

print("Solution to day 3 part 1:", len(doublec))
print("Solution to day 3 part 2:", allc-claimed)
