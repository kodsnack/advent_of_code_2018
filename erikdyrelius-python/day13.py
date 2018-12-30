inp = open("day13.txt").read()

def parseTracks(inp):
    tracks = dict()
    carts = list()
    for y, line in enumerate(inp.splitlines()):
        for x, c in enumerate(line):
            if c in '-/\\|+':
                tracks[(x,y)] = c
            elif c in '^>v<':
                facing = '^>v<'.find(c)
                carts.append((y, x, facing, 0))
                tracks[(x,y)] = '|-|-'[facing]
    return tracks, carts

def collision(carts):
    s = set()
    for cart in carts:
        if not cart:
            continue
        y, x, direction, status = cart
        if (x, y) in s:
            return (x, y)
        s.add((x, y))
    return False

def removeWreck(carts):
    for i in range(len(carts)-1):
        if not carts[i]:
            continue
        for j in range(i+1, len(carts)):
            if not carts[j]:
                continue
            if carts[i][:2] == carts[j][:2]:
                carts[i] = None
                carts[j] = None
                return carts

turns = {'\\':(3, 2, 1, 0), '/':(1, 0, 3, 2)}
steps = {0:(0, -1), 1:(1, 0), 2:(0, 1), 3:(-1, 0)}
inters = (3, 0, 1)
def takeStep(tracks, cart):
    if not cart:
        return None
    y, x, heading, status = cart
    theStep = steps[heading]
    x, y = x + theStep[0], y + theStep[1]
    track = tracks[(x, y)]
    if track in turns:
        heading = turns[track][heading]
    elif track == '+':
        heading = (heading + inters[status]) % 4
        status = (status + 1) % 3
    return (y, x, heading, status)

def step(t, d, removeWrecks=False):
    i = 0
    while i < len(d):
        d[i] = takeStep(t, d[i])
        if collision(d):
            if removeWrecks:
                removeWreck(d)
            else:
                break
        i += 1
    return sorted(filter(None, d))

tracks, carts = parseTracks(inp)
while not collision(carts):
    carts = step(tracks, carts)
theCollision = collision(carts)
print("Solution to day 12 part 1: {},{}".format(theCollision[0], 
                                                theCollision[1]))

tracks, carts = parseTracks(inp)
while len(carts) > 1:
    carts = step(tracks, carts, removeWrecks=True)
cart = carts[0]
print("Solution to day 12 part 2: {},{}".format(cart[1], cart[0]))
