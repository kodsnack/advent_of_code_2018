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

startingPattern, lookup = parseInput(inp)

cache = {}
print(lookup)
s = startingPattern
left = 0
sm1 = 0
for i in range(1,5000):
    s1 = '....' + s + '....'
    s3 = []
    for j in range(len(s1)-4):
        s2 = s1[j:j+5]
        s3.append(lookup[s2])
    s = ''.join(s3)
    left -= 2
    s1 = s.strip('.')
    left += s.find(s1)
    s = s1
    sm = 0
    l = left
    for c in s:
        if c=='#':
            sm += left
        left += 1
    left = l
    print(i, sm, sm-sm1, (50000000000-i)*(sm-sm1)+sm)
    sm1 = sm

print("Solution to day 12 part 1: {}".format(sm))
print("Solution to day 12 part 2: {}".format('12.2'))
