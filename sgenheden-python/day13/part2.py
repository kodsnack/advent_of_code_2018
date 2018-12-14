
import sys
import copy

from utils import read_input, print_world, turn_operation, rotation_operation, move_operation


def play(world, ncarts):

    while True and ncarts > 1:
        next_world = copy.deepcopy(world)
        collided = []
        for y, row in enumerate(world):
            for x, col in enumerate(row):
                if isinstance(col, tuple):
                    if (x, y) in collided:
                        continue
                    piece = col[0]
                    cart = col[1]
                    direction = cart[0]
                    turn = cart[1]
                    if piece == '+':
                        direction = turn_operation[turn][direction]
                        turn += 1
                        if turn == 3:
                            turn = 0
                    elif piece in ['\\', '/']:
                        direction = rotation_operation[piece][direction]
                    deltax, deltay = move_operation[piece][direction]
                    newx = x + deltax
                    newy = y + deltay
                    newcell = next_world[newy][newx]
                    if isinstance(newcell, tuple):
                        collided.append((newx, newy))
                        next_world[newy][newx] = newcell[0]
                        next_world[y][x] = col[0]
                        ncarts -= 2
                        print(f"Collision at {newx}, {newy} {ncarts}")
                        sys.stdout.flush()
                    else:
                        piece = next_world[newy][newx]
                        next_world[newy][newx] = (piece, (direction, turn))
                        next_world[y][x] = col[0]
        world = next_world

    for y, row in enumerate(world):
        for x, col in enumerate(row):
            if isinstance(col, tuple):
                print(f"Coordinate of remaining cart is {x}, {y}")


if __name__ == "__main__":
    world, ncarts = read_input()
    play(world, ncarts)
