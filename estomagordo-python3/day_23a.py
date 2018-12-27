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

    count = 0

    inrange = []

    for i, bot in enumerate(bots):
        c = 0
        if bot[3] != radiest:
            continue
        for j, bott in enumerate(bots):
            if distance(bot, bott) <= bot[3]:
                c += 1
        inrange.append(c)
    
    return inrange
    
    for i, bot in enumerate(bots):
        if distance(bot, bots[radiest_bot]) <= radiest:
            count += 1

    return count


def read_and_solve():
    pattern = re.compile('-?\d+')
    with open('input_23.txt') as f:
        data = []
        for line in f:
            data.append([int(val) for val in re.findall(pattern, line) if val])
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())