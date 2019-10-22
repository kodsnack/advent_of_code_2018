import re
from heapq import heappop, heappush


def reaches_box(xmin, xmax, ymin, ymax, zmin, zmax, bot):
    x, y, z, r = bot

    ans = xmin <= x < xmax and ymin <= y < ymax and zmin <= z < zmax

    if not ans:
        if x < xmin:
            r -= (xmin - x)
        elif x >= xmax:
            r -= (x - xmax + 1)
        if y < ymin:
            r -= (ymin - y)
        elif y >= ymax:
            r -= (y - ymax + 1)
        if z < zmin:
            r -= (zmin - z)
        elif z >= zmax:
            r -= (z - zmax + 1)

        ans = r >= 0
        
    return ans


def solve(bots):
    xmin = min(bot[0] for bot in bots)
    xmax = max(bot[0] for bot in bots)
    ymin = min(bot[1] for bot in bots)
    ymax = max(bot[1] for bot in bots)
    zmin = min(bot[2] for bot in bots)
    zmax = max(bot[2] for bot in bots)
    
    frontier = [(-len(bots), xmin, ymin, zmin, xmax, ymax, zmax)]

    while frontier:
        _, xmin, ymin, zmin, xmax, ymax, zmax = heappop(frontier)

        if xmin == xmax - 1 and ymin == ymax  - 1 and zmin == zmax - 1:
            return xmin + ymin + zmin

        xranges = ((xmin, xmax),) if (xmin == xmax - 1) else ((xmin, (xmin + xmax) // 2), ((xmin + xmax) // 2, xmax))
        yranges = ((ymin, ymax),) if (ymin == ymax - 1) else ((ymin, (ymin + ymax) // 2), ((ymin + ymax) // 2, ymax))
        zranges = ((zmin, zmax),) if (zmin == zmax - 1) else ((zmin, (zmin + zmax) // 2), ((zmin + zmax) // 2, zmax))

        def do(xmin, xmax, ymin, ymax, zmin, zmax):
            return (-sum(reaches_box(xmin, xmax, ymin, ymax, zmin, zmax, bot) for bot in bots), xmin, ymin, zmin, xmax, ymax, zmax)

        for xr in xranges:
            for yr in yranges:
                for zr in zranges:
                    heappush(frontier, do(xr[0], xr[1], yr[0], yr[1], zr[0], zr[1]))
                    
    
def read_and_solve():
    pattern = re.compile('-?\d+')
    with open('input_23.txt') as f:
        data = []
        for line in f:
            data.append(tuple([int(val) for val in re.findall(pattern, line) if val]))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())