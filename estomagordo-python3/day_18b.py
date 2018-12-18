def get_neighbours(d, height, width, y, x):
    neighbours = []

    if y > 0:
        neighbours.append(d[y - 1][x])
        if x > 0:
            neighbours.append(d[y - 1][x - 1])
        if x < width - 1:
            neighbours.append(d[y - 1][x + 1])
    if y < height - 1:
        neighbours.append(d[y + 1][x])
        if x > 0:
            neighbours.append(d[y + 1][x - 1])
        if x < width - 1:
            neighbours.append(d[y + 1][x + 1])
    if x > 0:
        neighbours.append(d[y][x - 1])
    if x < width - 1:
        neighbours.append(d[y][x + 1])

    return neighbours


def solve(d):
    height = len(d)
    width = len(d[0])

    values = set()
    cycle = {}

    for i in range(10000):
        new_area = []

        for y, row in enumerate(d):
            new_row = []
            for x, c in enumerate(row):
                neighbours = get_neighbours(d, height, width, y, x)
                if c == '.':
                    if sum(n == '|' for n in neighbours) > 2:
                        new_row.append('|')
                    else:
                        new_row.append('.')
                if c == '|':
                    if sum(n == '#' for n in neighbours) > 2:
                        new_row.append('#')
                    else:
                        new_row.append('|')
                if c == '#':
                    if sum(n == '#' for n in neighbours) > 0 and sum(n == '|' for n in neighbours) > 0:
                        new_row.append('#')
                    else:
                        new_row.append('.')
            new_area.append(new_row)

        d = new_area

        if i > 1000:
            score = sum(sum(c == '#' for c in line) for line in d) * sum(sum(c == '|' for c in line) for line in d)
            values.add(score)
            if i > 1500:
                cycle[i % len(values)] = score
            if i == 1501 + len(values):
                return cycle[999999999 % len(values)]


def read_and_solve():
    with open('input_18.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())