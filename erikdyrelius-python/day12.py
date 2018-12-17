from aocbase import readInput

inp = readInput()

def lineParse(s, f=lambda x:x):
    return s.split()

def fileParse(inp):
    s = inp.splitlines()
    ss = list(map(lambda x: lineParse(x), s[2:]))
    return s[0][15:], ss

def parseInput(inp):
    first, patterns = fileParse(inp)
    lookup = dict()
    for pattern in patterns:
        lookup[pattern[0]] = pattern[2]
    return first, lookup

def generation(current, lookup):
    s1 = '....' + current + '....'
    ret = []
    for j in range(len(s1)-4):
        ret.append(lookup[s1[j:j+5]])
    ref = ''.join(ret)
    ret = ref.strip()
    return ret, ref.find(ret)-2

def potSum(current, left):
    sm = 0
    for c in current:
        if c=='#':
            sm += left
        left += 1
    return sm
    
def twentyGen(current, lookup):
    left = 0
    for i in range(20):
        current, deltaLeft = generation(current, lookup)
        left += deltaLeft
    return potSum(current, left)

def manyGen(current, lookup, n):
    left = 0
    gen = 0
    oldSm = 0
    oldSmDiff = 0
    while True:
        current, deltaLeft = generation(current, lookup)
        left += deltaLeft
        sm = potSum(current, left)
        gen += 1
        if sm-oldSm == oldSmDiff:
            return (50000000000-gen)*(sm-oldSm)+sm
        oldSmDiff = sm-oldSm
        oldSm = sm

startingPattern, lookup = parseInput(inp)

print("Solution to day 12 part 1: {}".format(twentyGen(startingPattern, 
                                                       lookup)))
print("Solution to day 12 part 2: {}".format(manyGen(startingPattern, 
                                                     lookup, 50000000000)))
