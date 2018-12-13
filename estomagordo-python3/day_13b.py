def solve(d):
    carts = {}
    cart_locations = {}

    for y, line in enumerate(d):
        for x, c in enumerate(line):
            if c not in '>v>^':
                continue
            number = len(carts)
            cart_locations[(x, y)] = number
            if c == '>':
                carts[number] = [0, 0]
            elif c == 'v':
                carts[number] = [1, 0]
            elif c == '<':
                carts[number] = [2, 0]
            else:
                carts[number] = [3, 0]
    steps = 0
    while True:
        location_list = sorted(cart_locations.keys(), key=lambda location: [location[1], location[0]])
        
        for x, y in location_list:
            cart_number = cart_locations[(x, y)]
            direction, mode = carts[cart_number]
            dx = x
            dy = y
            
            if direction == 0:
                dx += 1
            elif direction == 1:
                dy += 1
            elif direction == 2:
                dx -= 1
            else:
                dy -= 1

            if (dx, dy) in cart_locations:
                return dx, dy

            if d[dy][dx] == '/':
                if direction == 0:
                    direction = 3
                elif direction == 1:
                    direction = 2
                elif direction == 2:
                    direction = 1
                else:
                    direction = 0
            elif d[dy][dx] == '\\':
                if direction == 0:
                    direction = 1
                elif direction == 1:
                    direction = 0
                elif direction == 2:
                    direction = 3
                else:
                    direction = 2
            elif d[dy][dx] == '+':
                if mode == 0:
                    direction = (direction - 1) % 4
                elif mode == 2:
                    direction = (direction + 1) % 4
                mode = (mode + 1) % 3
            
            del cart_locations[(x, y)]
            carts[cart_number] = [direction, mode]
            cart_locations[(dx, dy)] = cart_number
        steps += 1

def read_and_solve():
    with open('input_13.txt') as f:
        data = [line[:-1] for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())