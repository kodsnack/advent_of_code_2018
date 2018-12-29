"""
December 13, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from enum import Enum
from common.puzzlesolver import PuzzleSolver


class Turn(Enum):
    LEFT = 1
    STRAIGHT = 2
    RIGHT = 3

    def next_time(self):
        if self == self.LEFT:
            return self.STRAIGHT
        elif self == self.STRAIGHT:
            return self.RIGHT
        else:
            return self.LEFT


class Direction(Enum):
    EAST = 0
    NORTH = 1
    WEST = 2
    SOUTH = 3
    LAST = 4

    def turn_left(self):
        return Direction((self.value + 1) % self.LAST.value)

    def turn_right(self):
        return Direction((self.value - 1) % self.LAST.value)

    def turn(self, turn):
        if turn == turn.LEFT:
            return self.turn_left()
        elif turn == turn.RIGHT:
            return self.turn_right()
        # Go straight.
        return self


class Cart:
    def __init__(self, x, y, direction):
        self.x = x
        self.y = y
        self.direction = direction
        self.next_turn = Turn.LEFT
        self.collided = False

    def collide(self):
        self.collided = True
        self.x = None
        self.y = None
        self.direction = None
        self.next_turn = None

    def move(self):
        if self.direction == self.direction.EAST:
            self.x += 1
        elif self.direction == self.direction.WEST:
            self.x -= 1
        elif self.direction == self.direction.SOUTH:
            self.y += 1
        else:
            self.y -= 1

    def turn(self):
        self.direction = self.direction.turn(self.next_turn)
        self.next_turn = self.next_turn.next_time()

    def turn_left(self):
        self.direction = self.direction.turn_left()

    def turn_right(self):
        self.direction = self.direction.turn_right()


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    def read_grid(self):
        grid = []
        for line in self.lines(strip=False):
            grid.append(line)
        return grid

    @staticmethod
    def find_carts(grid):
        carts_ix = 0
        carts = []
        carts_pos = {}
        x = 0
        y = 0

        while True:
            if y >= len(grid):
                break
            x1 = grid[y].find("<", x)
            x2 = grid[y].find(">", x)
            x3 = grid[y].find("^", x)
            x4 = grid[y].find("v", x)
            if x1 < 0 and x2 < 0 and x3 < 0 and x4 < 0:
                x = 0
                y += 1
                continue

            for xs in sorted([x1, x2, x3, x4]):
                if xs >= 0:
                    x = xs
                    break

            if grid[y][x] == "<":
                carts.append(Cart(x, y, Direction.WEST))
            elif grid[y][x] == ">":
                carts.append(Cart(x, y, Direction.EAST))
            elif grid[y][x] == "v":
                carts.append(Cart(x, y, Direction.SOUTH))
            elif grid[y][x] == "^":
                carts.append(Cart(x, y, Direction.NORTH))
            else:
                print("What!??", grid[y][x])
                return

            carts_pos[(x, y)] = carts_ix
            carts_ix += 1
            x += 1
        return (carts, carts_pos)

    @staticmethod
    def update(cart, lines):
        old_x = cart.x
        old_y = cart.y

        cart.move()
        x = cart.x
        y = cart.y

        if cart.direction == Direction.EAST or cart.direction == Direction.WEST:
            if lines[y][x] == "/":
                cart.turn_left()
            elif lines[y][x] == "\\":
                cart.turn_right()
            elif lines[y][x] == "+":
                cart.turn()
        elif cart.direction == Direction.NORTH or cart.direction == Direction.SOUTH:
            if lines[y][x] == "/":
                cart.turn_right()
            elif lines[y][x] == "\\":
                cart.turn_left()
            elif lines[y][x] == "+":
                cart.turn()

    def solve_part_one(self):
        """Solution for part one."""
        grid = self.read_grid()
        carts, carts_pos = self.find_carts(grid)

        while True:
            sorted_pos = sorted(carts_pos)
            for x, y in sorted_pos:
                cart_ix = carts_pos[(x, y)]
                if cart_ix is None:
                    continue
                cart = carts[cart_ix]
                self.update(cart, grid)
                if (cart.x, cart.y) in carts_pos and carts_pos[
                    (cart.x, cart.y)
                ] is not None:
                    return (cart.x, cart.y)
                carts_pos[(x, y)] = None
                carts_pos[(cart.x, cart.y)] = cart_ix

            # Delete soft removed carts.
            carts_pos = {k: v for k, v in carts_pos.items() if v is not None}

    def solve_part_two(self):
        """Solution for part two."""
        grid = self.read_grid()
        carts, carts_pos = self.find_carts(grid)

        while len(carts_pos) > 1:
            sorted_pos = sorted(carts_pos)
            for x, y in sorted_pos:
                cart_ix = carts_pos[(x, y)]
                if cart_ix is None:
                    continue
                cart = carts[cart_ix]
                self.update(cart, grid)
                if (cart.x, cart.y) in carts_pos and carts_pos[
                    (cart.x, cart.y)
                ] is not None:
                    carts_pos[(x, y)] = None
                    carts_pos[(cart.x, cart.y)] = None
                    cart.collide()
                    continue
                carts_pos[(x, y)] = None
                carts_pos[(cart.x, cart.y)] = cart_ix

            # Delete soft removed carts.
            carts_pos = {k: v for k, v in carts_pos.items() if v is not None}

        return list(carts_pos)[0]

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december13.input", strip=False)
    one = s.solve_part_one()
    print("{:d},{:d}".format(one[0], one[1]))
    two = s.solve_part_two()
    print("{:d},{:d}".format(two[0], two[1]))
