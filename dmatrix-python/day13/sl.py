def create_cart(x, y, symbol):
    return [x, y, 0, {'>': 0, 'v': 1, '<': 2, '^': 3}.get(symbol)]


def turn(symbol, direction, mode):
    if symbol == '/':
        if direction == 0:
            direction = 3
        elif direction == 1:
            direction = 2
        elif direction == 2:
            direction = 1
        else:
            direction = 0
    elif symbol == '\\':
        if direction == 0:
            direction = 1
        elif direction == 1:
            direction = 0
        elif direction == 2:
            direction = 3
        else:
            direction = 2
    elif symbol == '+':
        if mode == 0:
            direction = (direction - 1) % 4
        elif mode == 2:
            direction = (direction + 1) % 4
        mode = (mode + 1) % 3

    return direction, mode


def move(x, y, direction):
    if direction == 0:
        x += 1
    elif direction == 1:
        y += 1
    elif direction == 2:
        x -= 1
    else:
        y -= 1

    return x, y


def part1(lines):
    carts = []

    for y, line in enumerate(lines):
        for x, symbol in enumerate(line):
            if symbol in '<v>^':
                carts.append(create_cart(x, y, symbol))

    while True:
        moved_carts = set()
        carts.sort(key=lambda x: [x[1], x[0]])

        for cart_idx, cart in enumerate(carts):
            x, y, mode, direction = cart
            x, y = move(x, y, direction)

            for cn, _cart in enumerate(carts):
                if _cart[:2] == [x, y]:
                    if cn in moved_carts or (_cart[3] != direction and
                                             not (_cart[3] + direction) % 2):
                        return ','.join(map(str, [x, y]))

            direction, mode = turn(lines[y][x], direction, mode)

            carts[cart_idx] = [x, y, mode, direction]
            moved_carts.add(cart_idx)


def solve():
    with open("input.txt") as f:
        lines = [line[:-1] for line in f.readlines()]
        print(f"Answer for the first part: \n{part1(lines)}")
        #print(f"Answer for the second part: \n{part2(lines)}")


if __name__ == '__main__':
    solve()
