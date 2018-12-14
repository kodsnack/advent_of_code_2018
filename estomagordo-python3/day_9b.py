def solve(d):
    playercount = int(d[0])
    marblecount = int(d[6])

    elves = [0] * playercount
    marbles = { 0: [0, 0] }
    current = 0
    currelf = 0

    def move(curr, clockwise, steps):
        for _ in range(steps):
            if clockwise:
                curr = marbles[curr][0]
            else:
                curr = marbles[curr][-1]
        return curr

    for marble in range(1, marblecount * 100 + 1):
        if marble % 23 > 0:
            current = move(current, True, 1)
            nextmarb = move(current, True, 1)
            marbles[current][0] = marble
            marbles[marble] = [nextmarb, current]
            marbles[nextmarb][1] = marble
            current = marble
        else:
            elves[currelf] += marble
            current = move(current, False, 7)
            nextmarb = move(current, True, 1)
            prevmarb = move(current, False, 1)
            elves[currelf] += current
            marbles[prevmarb][0] = nextmarb
            marbles[nextmarb][1] = prevmarb
            current = nextmarb
        
        currelf = (currelf + 1) % len(elves)

    return max(elves)


def read_and_solve():
    with open('input_9.txt') as f:
        data = [line.split() for line in f]
        return solve(data[0])

if __name__ == '__main__':
    print(read_and_solve())