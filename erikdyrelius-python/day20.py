from aocbase import readInput

direction = {'N':(0, -1), 'E':(1, 0), 'S':(0, 1), 'W':(-1, 0)}

def parse(s, dist, x, y):
    while len(s) > 0:
        if s[0] in direction:
            nxtX, nxtY = (x + direction[s[0]][0], y + direction[s[0]][1])
            dist[nxtX, nxtY] = min(dist[x, y]+1, dist.get((nxtX, nxtY), 10**9))
            x, y = nxtX, nxtY
        elif s[0] == '^':
            dist[x, y] = 0
            while s[0] != '$':
                s = parse(s[1:], dist, x, y)
        elif s[0] == '(':
            while s[0] != ')':
                s = parse(s[1:], dist, x, y)
        elif s[0] in ')$|':
            return s
        s = s[1:]
    return dist

d = dict()
inp = readInput()
parse(inp, d, 0, 0)
print("Solution to day 20 part 1:",max(d.values()))
print("Solution to day 20 part 2:",len(list(filter(lambda x:x>=1000, d.values()))))
