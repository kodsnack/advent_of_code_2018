from aocbase import readInput
import re

inp = readInput()
#inp = "dabAcCaCBAcCcaDA"
def estrip(inp):
    change = True
    s = list(inp)
    while change:
        change = False
        i = 0
        n = []
        skip = False
        for c1, c2 in zip(s, s[1:]+[' ']):
            if skip == True:
                skip = False
                continue
            if c1 != c2 and c1.lower() == c2.lower():
                skip = True
                change = True
            else:
                n.append(c1)
        s = n
    return s
resPolymer = estrip(inp)
print("Solution to day 5 part 1: {}".format(len(resPolymer)))

unitTypes = set(map(lambda x:x.lower(), resPolymer))
minUnitStrLen = len(inp)
for unitType in unitTypes:
    impPolymer = ''.join(map(lambda x,ut=unitType:'' if x.lower()==ut else x, resPolymer))
    impPolymerTry = estrip(impPolymer)
    minUnitStrLen = min(minUnitStrLen, len(impPolymerTry))
print("Solution to day 5 part 2:", minUnitStrLen)
