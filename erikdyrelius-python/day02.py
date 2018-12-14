from aocbase import readInput

inp = readInput()

def parseTokens(inp, f):
    for line in inp.strip().splitlines():
        for token in line.split():
            yield f(token)

s = list(parseTokens(inp, str))
sm2, sm3 = 0,0
for t in s:
    f2, f3 = 0,0
    for i in t:
        if t.count(i) == 2: f2 = 1
        if t.count(i) == 3: f3 = 1
    sm2 += f2
    sm3 += f3
print("Solution to day 2 part 1:", sm2*sm3)
for i in range(len(s[0])):
    t2 = sorted([x[:i]+x[i+1:] for x in s])
    for x, y in zip(t2[1:], t2[:-1]):
        if x==y: print("Solution to day 1 part 2:", x)
