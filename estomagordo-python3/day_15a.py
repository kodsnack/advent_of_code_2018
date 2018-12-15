def get_neighbours(x, y, width, height):
    neighbours = []

    if x > 0:
        neighbours.append([x - 1, y])
    if x < width - 1:
        neighbours.append([x + 1, y])
    if y > 0:
        neighbours.append([x, y - 1])
    if y < height - 1:
        neighbours.append([x, y + 1])

    return neighbours


def find_target(d, width, height, x, y, target):
    targets = {}

    seen = {}

    frontier = []

    if y > 0 and d[y - 1][x] == '.':
        frontier.append([x, y - 1, 1])
    if y < height - 1 and d[y + 1][x] == '.':
        frontier.append([x, y + 1, 1])
    if x > 0 and d[y][x - 1] == '.':
        frontier.append([x - 1, y, 1])
    if x < width - 1 and d[y][x + 1] == '.':
        frontier.append([x + 1, y, 1])

def distance_to_target(d, width, height, x, y, target):
    if d[y][x] != '.':
        return 100000
    if not 0 <= x < width:
        return 100000
    if not 0 <= y < height:
        return 100000
    if d[y][x] == '#':
        return 100000
    
    frontier = [[x, y, 0]]
    pos = 0
    seen = set()

    while pos < len(frontier):
        vx, vy, distance = frontier[pos]
        
        if (vx, vy) in seen:
            pos += 1
            continue

        seen.add((vx, vy))
        neighbours = get_neighbours(vx, vy, width, height)

        for nx, ny in neighbours:
            if d[ny][nx] == target:
                return distance
            elif d[ny][nx] == '.' and not (nx, ny) in seen:
                frontier.append([nx, ny, distance + 1])

        pos += 1

    return 100000


def solve(d):
    height = len(d)
    width = len(d[0])

    starting_hit_points = 200
    attack_power = 3
    turns = 0

    elves = {}
    goblins = {}

    for y, line in enumerate(d):
        for x, c in enumerate(line):
            if c == 'E':
                elves[(x, y)] = [starting_hit_points, False]
            if c == 'G':
                goblins[(x, y)] = [starting_hit_points, False]

    while True:
        for _, elf in elves.items():
            elf[1] = False
        for _, goblin in goblins.items():
            goblin[1] = False
        for y, line in enumerate(d):
            for x, c in enumerate(line):
                if c not in 'EG':
                    continue

                is_elf = d[y][x] == 'E'

                if (is_elf and elves[(x, y)][1]) or (not is_elf and goblins[(x, y)][1]):
                    continue

                target_type = 'G' if is_elf else 'E'
                moving_dict = elves if is_elf else goblins
                target_dict = goblins if is_elf else elves

                if (is_elf and not goblins) or (not is_elf and not elves):
                    hitsum = 0
                    for hits, _ in elves.values():
                        hitsum += hits
                    for hits, _ in goblins.values():
                        hitsum += hits

                    return hitsum * turns

                possible_targets = get_neighbours(x, y, width, height)
                targets = [p for p in possible_targets if ((p[0], p[1]) in goblins and is_elf) or ((p[0], p[1]) in elves and not is_elf)]

                if targets:
                    weakest = 1000000
                    if y > 0 and d[y - 1][x] == target_type:
                        weakest = min(weakest, target_dict[(x, y - 1)][0])
                    if x > 0 and d[y][x - 1] == target_type:
                        weakest = min(weakest, target_dict[(x - 1, y)][0])
                    if x < width - 1 and d[y][x + 1] == target_type:
                        weakest = min(weakest, target_dict[(x + 1, y)][0])
                    if y < height - 1 and d[y + 1][x] == target_type:
                        weakest = min(weakest, target_dict[(x, y + 1)][0])


                    if x < width - 1 and d[y][x + 1] == target_type and target_dict[(x + 1, y)][0] == weakest:
                        target_dict[(x + 1, y)][0] -= attack_power
                        if target_dict[(x + 1, y)][0] <= 0:
                            del target_dict[(x + 1, y)]
                            d[y][x + 1] = '.'
                    elif y < height - 1 and d[y + 1][x] == target_type and target_dict[(x, y + 1)][0] == weakest:
                        target_dict[(x, y + 1)][0] -= attack_power
                        if target_dict[(x, y + 1)][0] <= 0:
                            del target_dict[(x, y + 1)]
                            d[y + 1][x] = '.'
                    elif y > 0 and d[y - 1][x] == target_type and target_dict[(x, y - 1)][0] == weakest:
                        target_dict[(x, y - 1)][0] -= attack_power
                        if target_dict[(x, y - 1)][0] <= 0:
                            del target_dict[(x, y - 1)]
                            d[y - 1][x] = '.'
                    elif x > 0 and d[y][x - 1] == target_type and target_dict[(x - 1, y)][0] == weakest:
                        target_dict[(x - 1, y)][0] -= attack_power
                        if target_dict[(x - 1, y)][0] <= 0:
                            del target_dict[(x - 1, y)]
                            d[y][x - 1] = '.'

                else:
                    up_dist = distance_to_target(d, width, height, x, y - 1, target_type)
                    left_dist = distance_to_target(d, width, height, x - 1, y, target_type)
                    right_dist = distance_to_target(d, width, height, x + 1, y, target_type)
                    down_dist = distance_to_target(d, width, height, x, y + 1, target_type)

                    closest_dist = min([up_dist, left_dist, right_dist, down_dist])

                    if closest_dist == 100000:
                        continue

                    newx = x
                    newy = y

                    if up_dist == closest_dist:
                        hits, _ = elves[(x, y)] if is_elf else goblins[(x, y)]
                        del moving_dict[(x, y)]
                        moving_dict[(x, y - 1)] = [hits, True]
                        d[y][x] = '.'
                        d[y - 1][x] = c
                        newy -= 1
                    elif left_dist == closest_dist:
                        hits, _ = elves[(x, y)] if is_elf else goblins[(x, y)]
                        del moving_dict[(x, y)]
                        moving_dict[(x - 1, y)] = [hits, True]
                        d[y][x] = '.'
                        d[y][x  - 1] = c
                        newx -= 1
                    elif right_dist == closest_dist:
                        hits, _ = elves[(x, y)] if is_elf else goblins[(x, y)]
                        del moving_dict[(x, y)]
                        moving_dict[(x + 1, y)] = [hits, True]
                        d[y][x] = '.'
                        d[y][x + 1] = c
                        newx += 1
                    else:
                        hits, _ = elves[(x, y)] if is_elf else goblins[(x, y)]
                        del moving_dict[(x, y)]
                        moving_dict[(x, y + 1)] = [hits, True]
                        d[y][x] = '.'
                        d[y + 1][x] = c
                        newy += 1

                    possible_targets = get_neighbours(newx, newy, width, height)
                    targets = [p for p in possible_targets if ((p[0], p[1]) in goblins and is_elf) or ((p[0], p[1]) in elves and not is_elf)]

                    if targets:
                        weakest = 1000000
                        if newy > 0 and d[newy - 1][newx] == target_type:
                            weakest = min(weakest, target_dict[(newx, newy - 1)][0])
                        if newx > 0 and d[newy][newx - 1] == target_type:
                            weakest = min(weakest, target_dict[(newx - 1, newy)][0])
                        if newx < width - 1 and d[newy][newx + 1] == target_type:
                            weakest = min(weakest, target_dict[(newx + 1, newy)][0])
                        if newy < height - 1 and d[newy + 1][newx] == target_type:
                            weakest = min(weakest, target_dict[(newx, newy + 1)][0])

                        if newy > 0 and d[newy - 1][newx] == target_type and target_dict[(newx, newy - 1)][0] == weakest:
                            target_dict[(newx, newy - 1)][0] -= attack_power
                            if target_dict[(newx, newy - 1)][0] <= 0:
                                del target_dict[(newx, newy - 1)]
                                d[newy - 1][newx] = '.'
                        elif newx > 0 and d[newy][newx - 1] == target_type and target_dict[(newx - 1, newy)][0] == weakest:
                            target_dict[(newx - 1, newy)][0] -= attack_power
                            if target_dict[(newx - 1, newy)][0] <= 0:
                                del target_dict[(newx - 1, newy)]
                                d[newy][newx - 1] = '.'
                        elif newx < width - 1 and d[newy][newx + 1] == target_type and target_dict[(newx + 1, newy)][0] == weakest:
                            target_dict[(newx + 1, newy)][0] -= attack_power
                            if target_dict[(newx + 1, newy)][0] <= 0:
                                del target_dict[(newx + 1, newy)]
                                d[newy][newx + 1] = '.'
                        else:
                            target_dict[(newx, newy + 1)][0] -= attack_power
                            if target_dict[(newx, newy + 1)][0] <= 0:
                                del target_dict[(newx, newy + 1)]
                                d[newy + 1][newx] = '.'

        turns += 1


def read_and_solve():
    with open('input_15_small_4.txt') as f:
        data = [list(line.rstrip()) for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())