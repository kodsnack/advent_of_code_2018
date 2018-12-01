from aocbase import readInput

inp = readInput()

def parseTokens(inp, f):
    for line in inp.strip().splitlines():
        for token in line.split():
            yield f(token)

def iterUntilRepeat(inp):
    s = set()
    sm = 0
    while True:
        for i in parseTokens(inp, int):
            sm += i
            if sm in s:
                return sm
            else:
                s.add(sm)

print("Solution to day 1 part 1:", sum(parseTokens(inp, int)))
print("Solution to day 1 part 2:", iterUntilRepeat(inp))
