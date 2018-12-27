from heapq import heappop, heappush
from random import randint
import re


def distance(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1]) + abs(a[2] - b[2])


def solve(bots):
    radiest = 0
    radiest_bot = 0

    for i, bot in enumerate(bots):
        if bot[3] > radiest:
            radiest = bot[3]
            radiest_bot = i

    radius = 5
    counts = []

    for x in range(-radius, radius):
        for y in range(-radius, radius):
            for z in range(-radius, radius):
                count = 0
                for bot in bots:
                    if distance((x, y, z), bot) <= bot[3]:
                        count += 1
                counts.append(count)

    margin = 0.25

    xmin = min(bot[0] for bot in bots)
    xmax = max(bot[0] for bot in bots)
    ymin = min(bot[1] for bot in bots)
    ymax = max(bot[1] for bot in bots)
    zmin = min(bot[2] for bot in bots)
    zmax = max(bot[2] for bot in bots)

    xlo = int(xmin + margin * (xmax - xmin))
    xhi = int(xmax - margin * (xmax - xmin))
    ylo = int(ymin + margin * (ymax - ymin))
    yhi = int(ymax - margin * (ymax - ymin))
    zlo = int(zmin + margin * (zmax - zmin))
    zhi = int(zmax - margin * (zmax - zmin))

    heuristic_minx = 10**10
    heuristic_maxx = -10**10
    heuristic_miny = 10**10
    heuristic_maxy = -10**10
    heuristic_minz = 10**10
    heuristic_maxz = -10**10

    # xmin = 42244717
    # xmax = 58408256
    # ymin = 45869380
    # ymax = 62197448
    # zmin = 35279722
    # zmax = 51277767

    # xmin = 52873112
    # xmax = 58253538
    # ymin = 47044686
    # ymax = 51782859
    # zmin = 37229323
    # zmax = 42232336

    # xmin = 56621557
    # xmax = 58213471
    # ymin = 47085271
    # ymax = 48874455
    # zmin = 37263058
    # zmax = 38990982

    # xmin = 42244717
    # xmax = 57780560
    # ymin = 45869380
    # ymax = 47822320
    # zmin = 35279722
    # zmax = 38326612

    # xmin = 56621557
    # xmax = 57603714
    # ymin = 45869380
    # ymax = 47552654
    # zmin = 35279722
    # zmax = 37847458

    xmin = 57087369
    xmax = 57603388
    ymin = 47044923
    ymax = 47552626
    zmin = 37324796
    zmax = 37843308

    golower = 500000
    best = 925
    iterations = 0

    while True:
        x = randint(xmin - golower, xmax)
        y = randint(ymin - golower, ymax)
        z = randint(zmin - golower, zmax)

        count = 0

        for bot in bots:
            if distance((x, y, z), bot) <= bot[3]:
                count += 1

        if count >= best:
            best = count
            print(x, y, z, count, x + y + z)

        iterations += 1
        if iterations % 100000 == 0:
            print(iterations)

    best = 10**10

    found = []

    for _ in range(200):
        print(_)
        found = []

        while len(found) < 300:
            x = randint(xmin - golower, xmax)
            y = randint(ymin - golower, ymax)
            z = randint(zmin - golower, zmax)
            # x = randint(xmin - golower, xmin)
            # y = randint(ymin - golower, ymin)
            # z = randint(zmin - golower, zmin)

            count = 0

            for bot in bots:
                if distance((x, y, z), bot) <= bot[3]:
                    count += 1

            if count > 896:
                if x + y + z < best:
                    print(x + y + z)
                    best = x + y + z
                found.append((x, y, z))
                # heuristic_minx = min(heuristic_minx, x)
                # heuristic_maxx = max(heuristic_maxx, x)
                # heuristic_miny = min(heuristic_miny, y)
                # heuristic_maxy = max(heuristic_maxy, y)
                # heuristic_minz = min(heuristic_minz, z)
                # heuristic_maxz = max(heuristic_maxz, z)

                # heappush(best, (-count, x, y, z))

        xmin = min(xmin, min(f[0] for f in found))
        xmax = max(xmax, min(f[0] for f in found))
        ymin = min(ymin, min(f[1] for f in found))
        ymax = max(ymax, min(f[1] for f in found))
        zmin = min(zmin, min(f[2] for f in found))
        zmax = max(zmax, min(f[2] for f in found))

    print(xmin)
    print(xmax)
    print(ymin)
    print(ymax)
    print(zmin)
    print(zmax)

    print(xmax - xmin)
    print(ymax - ymin)
    print(zmax - zmin)

    return
    # best = 10**12

    # for x in range(xmin, xmax + 1):
    #     if x + ymin + zmin >= best:
    #         break
    #     for y in range(ymin, ymax + 1):
    #         print(x, y)
    #         if x + y + zmin >= best:
    #             break
    #         for z in range(zmin, zmax + 1):
    #             if x + y + z >= best:
    #                 break
    #             count = 0
    #             for bot in bots:
    #                 d = distance((x, y, z), bot)
    #                 if d <= bot[3]:
    #                     count += 1
    #             if count == 897:
    #                 if x + y + z < best:
    #                     best = x + y + z
    #                     print(best)
    # return
    # for _ in range(10000):
    #     best = []

    #     while len(best) < 10:
    #         x = randint(xmin, xmax)
    #         y = randint(ymin, ymax)
    #         z = randint(zmin, zmax)

    #         count = 0

    #         for bot in bots:
    #             if distance((x, y, z), bot) <= bot[3]:
    #                 count += 1

    #         if count > 896:
    #             heuristic_minx = min(heuristic_minx, x)
    #             heuristic_maxx = max(heuristic_maxx, x)
    #             heuristic_miny = min(heuristic_miny, y)
    #             heuristic_maxy = max(heuristic_maxy, y)
    #             heuristic_minz = min(heuristic_minz, z)
    #             heuristic_maxz = max(heuristic_maxz, z)

    #             heappush(best, (-count, x, y, z))

    #     xmax = heuristic_maxx
    #     ymax = heuristic_maxy
    #     zmax = heuristic_maxz

    while best:
        negcount, x, y, z = heappop(best)
        print(-negcount, x, y, z)

    # return heuristic_minx, heuristic_maxx, heuristic_miny, heuristic_maxy, heuristic_minz, heuristic_maxz

    print(heuristic_minx)
    print(heuristic_maxx)
    print(heuristic_miny)
    print(heuristic_maxy)
    print(heuristic_minz)
    print(heuristic_maxz)

    print(heuristic_maxx - heuristic_minx)
    print(heuristic_maxy - heuristic_miny)
    print(heuristic_maxz - heuristic_minz)

def read_and_solve():
    pattern = re.compile('-?\d+')
    with open('input_23.txt') as f:
        data = []
        for line in f:
            data.append([int(val) for val in re.findall(pattern, line) if val])
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())