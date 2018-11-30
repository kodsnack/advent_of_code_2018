from aocbase import readInput

inp = readInput()

def parseTokens(inp, f):
    for line in inp.strip().splitlines():
        for token in line.split():
            f(token)

parseTokens(inp, print)
