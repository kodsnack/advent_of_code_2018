from collections import defaultdict


def solve(d):
    moved = defaultdict(int)
    carts = []

    for y, line in enumerate(d):
        for x, c in enumerate(line):
            if c not in '>v<^':
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
                
    while True:
        mv = set()
        carts.sort(key=lambda cart: [cart[1], cart[0]])
        
        for cart_number, cart in enumerate(carts):
            x, y, mode, direction = cart
            
            if direction == 0:
                x += 1
            elif direction == 1:
                y += 1
            elif direction == 2:
                x -= 1
            else:
                y -= 1

            moved[cart_number] += 1

            for cn, cart in enumerate(carts):
                if cart[:2] == [x, y]:
                    if cn in mv or (cart[3] != direction and not (cart[3] + direction) % 2):
                        return ','.join(map(str, [x, y]))

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
            mv.add(cart_number)

def read_and_solve():
    with open('input_13.txt') as f:
        data = [line[:-1] for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
