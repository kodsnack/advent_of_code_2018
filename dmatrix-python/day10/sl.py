import re


def parse(lines):
    d = []
    for line in lines:
        d.append(
            list(map(int, list(filter(None, re.findall(r"-?\d*", line))))))
    return d


def move(d):
    for i in range(len(d)):
        mx, my = d[i][2], d[i][3]
        d[i][0] += mx
        d[i][1] += my


def pp(d, maxx, minx, maxy, miny):
    set_of_points = set()
    for x, y, dx, dy in d:
        set_of_points.add((x, y))

    ln = []
    for y in range(maxy - miny + 1):
        ln.append(['.'] * (maxx - minx + 1))

    for x in range(minx, maxx + 1):
        for y in range(miny, maxy + 1):
            if (x, y) in set_of_points:
                ln[y - miny][x - minx] = '#'

    return '\n'.join(''.join(c for c in line) for line in ln)


def part1(d):
    letter_height = 10
    while True:
        move(d)
        maxx = max(pnt[0] for pnt in d)
        minx = min(pnt[0] for pnt in d)
        maxy = max(pnt[1] for pnt in d)
        miny = min(pnt[1] for pnt in d)

        if maxy - miny < letter_height:
            return pp(d, maxx, minx, maxy, miny)


def part2(d):
    letter_height = 10
    sec = 0
    while True:
        move(d)
        sec += 1
        maxy = max(pnt[1] for pnt in d)
        miny = min(pnt[1] for pnt in d)

        if maxy - miny < letter_height:
            return sec


if __name__ == "__main__":
    with open("input.txt") as f:
        d = parse(f.readlines())
        print(f"Answer for the first part: \n{part1(d)}")
    with open("input.txt") as f:
        d = parse(f.readlines())
        print(f"Answer for the second part: {part2(d)}")
