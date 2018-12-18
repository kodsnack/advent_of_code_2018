
import fileinput
import sys


def read_input():
    def parse_coord_str(str):
        _, spec = str.split("=")
        range_lst = spec.split("..")
        if len(range_lst) == 1:
            range_lst.append(range_lst[0])
        return range(int(range_lst[0]), int(range_lst[1])+1)

    world = {}
    for line in fileinput.input():
        str1, str2 = line.strip().split(", ")
        if str1.startswith('x'):
            xrange = parse_coord_str(str1)
            yrange = parse_coord_str(str2)
        else:
            xrange = parse_coord_str(str2)
            yrange = parse_coord_str(str1)
        for x in xrange:
            for y in yrange:
                world[(x, y)] = "clay"
    return world


def boundaries(world):
    x_min = min(k[0] for k in world.keys())
    x_max = max(k[0] for k in world.keys())
    y_min = min(k[1] for k in world.keys())
    y_max = max(k[1] for k in world.keys())
    return (x_min-1, x_max+1),(y_min, y_max)


def print_world(world):

    boundx, boundy = boundaries(world)
    for y in range(boundy[0], boundy[1]+1):
        for x in range(boundx[0], boundx[1]+1):
            val = world.get((x, y), None)
            if val == 'clay':
                sys.stdout.write('#')
            elif val == 'spring':
                sys.stdout.write('+')
            elif val == 'water':
                sys.stdout.write('~')
            elif val == 'water2':
                sys.stdout.write('|')
            else:
                sys.stdout.write('.')
        print()


def find_y_stop(world, maxy, x, y):
    while (x, y) not in world:
        world[(x, y)] = 'water2'
        if y == maxy:
            return y
        y += 1
    if world[(x, y)] == 'clay':
        return y - 1
    else:
        return y


def find_stop(world, x_within, startx, y, increment):
    x = startx
    while x_within(x):
        if world.get((x, y), '') == 'clay':
            return x
        elif (x,y+1) not in world or world[(x, y+1)] == 'water2':
            return None
        x += increment
    return None


def find_tipping(world, x_within, startx, y, increment):
    x = startx
    while x_within(x):
        if (x, y+1) not in world:
            return x
        elif world[(x, y+1)] == 'water2':
            return None
        x += increment
    return x


def fill_stream(world, boundx, boundy, startx, starty):
    y = find_y_stop(world, boundy[1], startx, starty)
    if y == boundy[1]:
        return

    while True:
        stop_left = find_stop(world, lambda x: x >= boundx[0], startx-1, y, -1)
        stop_right = find_stop(world, lambda x: x <= boundx[1], startx+1, y, 1)
        if stop_left is None or stop_right is None:
            break
        for x in range(stop_left+1, stop_right):
            world[(x, y)] = 'water'
        y -= 1
        if y == 0:
            return

    if stop_left is not None:
        for x in range(stop_left+1, startx+1):
            world[(x, y)] = 'water2'
    if stop_right is not None:
        for x in range(startx, stop_right):
            world[(x, y)] = 'water2'

    if stop_left is None:
        foundx = find_tipping(world, lambda x: x >= boundx[0], startx-1, y, -1)
        if foundx is not None and foundx >= boundx[0]:
            for x in range(foundx, startx):
                world[(x,y)] = 'water2'
            fill_stream(world, boundx, boundy, foundx, y+1)

    stop_right = find_stop(world, lambda x: x <= boundx[1], startx + 1, y, 1)
    if stop_right is None:
        foundx = find_tipping(world, lambda x: x <= boundx[1], startx+1, y, 1)
        if foundx is not None and foundx <= boundx[1]:
            for x in range(startx, foundx+1):
                world[(x,y)] = 'water2'
            fill_stream(world, boundx, boundy, foundx, y+1)


if __name__ == "__main__":
    world = read_input()
    _, boundy = boundaries(world)
    miny = boundy[0]
    world[(500, 0)] = 'spring'
    boundx, boundy = boundaries(world)
    fill_stream(world, boundx, boundy, 500, 1)

    nwater = sum([1 for _, val in world.items() if val != 'clay'])
    print(f"Number of water tiles are {nwater-miny}")
    nwater = sum([1 for _, val in world.items() if val == 'water'])
    print(f"Number of water tiles that will not run out are {nwater}")
