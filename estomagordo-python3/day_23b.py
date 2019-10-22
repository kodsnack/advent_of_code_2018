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
    
    frontier = [(-len(bots), 0, xmin, xmax, ymin, ymax, zmin, zmax)]

    while frontier:
        potential, distance, xmin, xmax, ymin, ymax, zmin, zmax = heappop(frontier)

        if xmin == xmax - 1 and ymin == ymax  - 1 and zmin == zmax - 1:
            return xmin + ymin + zmin

        def rangemaker(lil, big):
            return ((lil, big),) if lil == big - 1 else ((lil, (lil + big) // 2), ((lil + big) // 2, big))

        xranges = rangemaker(xmin, xmax)
        yranges = rangemaker(ymin, ymax)
        zranges = rangemaker(zmin, zmax)

        def package(xmin, xmax, ymin, ymax, zmin, zmax):
            nearby = sum(reaches_box(xmin, xmax, ymin, ymax, zmin, zmax, bot) for bot in bots)
            dist = abs(xmin) + abs(ymin) + abs(zmin)
            return (-nearby, dist, xmin, xmax, ymin, ymax, zmin, zmax)

        for xr in xranges:
            for yr in yranges:
                for zr in zranges:
                    heappush(frontier, package(xr[0], xr[1], yr[0], yr[1], zr[0], zr[1]))

    
def read_and_solve():
    pattern = re.compile('-?\d+')
    with open('input_23.txt') as f:
        data = []
        for line in f:
            data.append(tuple([int(val) for val in re.findall(pattern, line) if val]))
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())