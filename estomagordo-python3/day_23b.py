import re


def reaches_box(xmin, xmax, ymin, ymax, zmin, zmax, bot):
    x, y, z, r = bot

    ans = xmin <= x <= xmax and ymin <= y <= ymax and zmin <= z <= zmax

    if not ans:
        if x < xmin:
            r -= (xmin - x)
        elif x > xmax:
            r -= (x - xmax)
        if y < ymin:
            r -= (ymin - y)
        elif y > ymax:
            r -= (y - ymax)
        if z < zmin:
            r -= (zmin - z)
        elif z > zmax:
            r -= (z - zmax)

        ans = r >= 0
        
    return ans


def solve(bots):
    xmin = min(bot[0] for bot in bots)
    xmax = max(bot[0] for bot in bots)
    ymin = min(bot[1] for bot in bots)
    ymax = max(bot[1] for bot in bots)
    zmin = min(bot[2] for bot in bots)
    zmax = max(bot[2] for bot in bots)
    
    lowest_diff = min(xmax - xmin, ymax - ymin, zmax - zmin)
    
    step = 1
    
    while step < lowest_diff:
        step *= 2
        
    step //= 4
    from collections import defaultdict
    d = defaultdict(int)
    
    def find(xmin, xmax, ymin, ymax, zmin, zmax, step, d, highest):
        d[step] += 1
        if step == 1:
            best = (0, 10**10)
            for x in range(xmin, xmax + 1, step):
                for y in range(ymin, ymax + 1, step):
                    for z in range(zmin, zmax + 1, step):
                        score = sum(reaches_box(x, x, y, y, z, z, bot) for bot in bots)
                        dist = abs(x) + abs(y) + abs(z)
                        if score > best[0] or (score == best[0] and dist < best[1]):
                            best = (score, dist)

            highest['highest'] = max(highest['highest'], best[0])
            return best

        candidates = []
        best = 0

        for x in range(xmin, xmax + 1, step):
            for y in range(ymin, ymax + 1, step):
                for z in range(zmin, zmax + 1, step):
                    score = sum(reaches_box(x, x + step, y, y + step, z, z + step, bot) for bot in bots)
                    if score > highest['highest']:
                        if score > best:
                            candidates = [(x, x + step, y, y + step, z, z + step)]
                            best = score
                        elif score == best:
                            candidates.append((x, x + step, y, y + step, z, z + step))

        values = [find(candidate[0], candidate[1], candidate[2], candidate[3], candidate[4], candidate[5], step // 2, d, highest) for candidate in candidates]
        values = [v for v in values if v]
        
        if values:
            best = values[0]

            for value in values[1:]:
                if value[0] > best[0] or (value[0] == best[0] and value[1] < best[1]):
                    best = value

            return best

    _, distance = find(xmin, xmax, ymin, ymax, zmin, zmax, step, d, {'highest': 0})

    return distance
    
def read_and_solve():
    pattern = re.compile('-?\d+')
    with open('input_23.txt') as f:
        data = []
        for line in f:
            data.append(tuple([int(val) for val in re.findall(pattern, line) if val]))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())