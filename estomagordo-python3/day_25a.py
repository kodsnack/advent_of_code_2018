def distance(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1]) + abs(a[2] - b[2]) + abs(a[3] - b[3])


def solve(d):
    constellations = {}

    for i, elem in enumerate(d):
        constellations[i] = [elem]
    
    while True:
        print(len(constellations))
        adding = [-1, -1]

        for keya, valuea in constellations.items():
            if adding[0] != -1:
                break
            for keyb, valueb in constellations.items():
                if keya == keyb:
                    continue

                found = False

                for itema in valuea:
                    if found:
                        break
                    for itemb in valueb:
                        if distance(itema, itemb) <= 3:
                            found = True
                            break

                if found:
                    adding = [keya, keyb]
                    break

        if adding[0] != -1:
            constellations[adding[0]] = constellations[adding[0]] + constellations[adding[1]]
            del constellations[adding[1]]
        else:
            break

    return len(constellations)


def read_and_solve():
    with open('input_25.txt') as f:
        data = [list(map(int, line.split(','))) for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())