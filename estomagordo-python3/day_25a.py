def distance(a, b):
    return sum(abs(a[x] - b[x]) for x in range(4))


def count_unique(constellations):
    return len({constellation[1] for constellation in constellations})


def solve(points):
    constellations = [[point, i] for i, point in enumerate(points)]

    while True:
        constellation_count = count_unique(constellations)

        for i in range(len(constellations)):
            for j in range(i + 1, len(constellations)):
                if constellations[i][1] == constellations[j][1]:
                    continue

                a = constellations[i][1]
                b = constellations[j][1]

                if distance(constellations[i][0], constellations[j][0]) <= 3:
                    for constellation in constellations:
                        if constellation[1] == a:
                            constellation[1] = b
                    break

        if constellation_count == count_unique(constellations):
            break

    return count_unique(constellations)


def read_and_solve():
    with open('input_25.txt') as f:
        data = [list(map(int, line.split(','))) for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())