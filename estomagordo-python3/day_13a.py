from collections import defaultdict


def solve(d):
    moved = defaultdict(int)
    carts = []

    for y, line in enumerate(d):
        for x, c in enumerate(line):
            if c not in '>v>^':
                continue
            carts.append([x, y, 0])
            if c == '>':
                carts[-1].append(0)
            elif c == 'v':
                carts[-1].append(1)
            elif c == '<':
                carts[-1].append(2)
            else:
                carts[-1].append(3)
    steps = 0
    while True:
        location_list = sorted([cart[:2] for cart in carts], key=lambda location: [location[1], location[0]])
        cart_order = [-1] * len(carts)
        for i in range(len(carts)):
            for j in range(len(carts)):
                if carts[j][:2] == location_list[i]:
                    cart_order[i] = j
                    break
        
        for cart_number in cart_order:
            x, y, mode, direction = carts[cart_number]
            
            if direction == 0:
                x += 1
            elif direction == 1:
                y += 1
            elif direction == 2:
                x -= 1
            else:
                y -= 1

            moved[cart_number] += 1

            if any(cart[:2] == [x, y] for cart in carts):
                return x, y

            if d[y][x] == '/':
                if direction == 0:
                    direction = 3
                elif direction == 1:
                    direction = 2
                elif direction == 2:
                    direction = 1
                else:
                    direction = 0
            elif d[y][x] == '\\':
                if direction == 0:
                    direction = 1
                elif direction == 1:
                    direction = 0
                elif direction == 2:
                    direction = 3
                else:
                    direction = 2
            elif d[y][x] == '+':
                if mode == 0:
                    direction = (direction - 1) % 4
                elif mode == 2:
                    direction = (direction + 1) % 4
                mode = (mode + 1) % 3            
            
            carts[cart_number] = [x, y, mode, direction]

        steps += 1

def read_and_solve():
    with open('input_13.txt') as f:
        data = [line[:-1] for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())