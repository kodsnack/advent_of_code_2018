import operator


def calculate(grid, x, y, serial_num):
    rack_id = x + 10
    power_level = rack_id * y + serial_num
    pl_multy = power_level * rack_id
    grid[(x,
          y)] = (0 if len(str(pl_multy)) < 3 else int(str(pl_multy)[-3])) - 5


def get_sum(grid, x, y, size):
    sm = 0
    for i in range(size):
        for inc in range(size):
            sm += grid[(x + inc, y + i)]
    return sm


def part2(serial_num):
    grid = {(j, i): 0 for j in range(1, 301) for i in range(1, 301)}
    for x in range(1, 301):
        for y in range(1, 301):
            calculate(grid, x, y, serial_num)
    best_score = -100000
    ret_str = ""
    for x in range(1, 301):
        for y in range(1, 301):
            score = grid[(x, y)]
            if score > best_score:
                best_score = score
                ret_str = f"{x},{y},1"
            for sz in range(2, 301):
                if x + sz > 301 or y + sz > 301:
                    break
                for dx in range(sz):
                    score += grid[(x + dx, y + sz - 1)]
                for dy in range(sz):
                    score += grid[(x + sz - 1, y + dy)]
                score -= grid[(x + sz - 1, y + sz - 1)]
                if score > best_score:
                    best_score = score
                    ret_str = str(x) + ',' + str(y) + ',' + str(sz)
                # print(ret_str)
    return ret_str


def part1(serial_num):
    grid = {(j, i): 0 for j in range(1, 301) for i in range(1, 301)}
    for x in range(1, 301):
        for y in range(1, 301):
            calculate(grid, x, y, serial_num)
    d = {}
    for x in range(1, 299):
        for y in range(1, 299):
            d.update({(x, y): get_sum(grid, x, y, 3)})
    return ",".join(
        list(map(str,
                 max(d.items(), key=operator.itemgetter(1))[0])))


if __name__ == "__main__":
    with open("input.txt") as f:
        d = int(f.readline().strip())
        print(f"Answer for the first part: \n{part1(d)}")
        print(f"Answer for the second part: \n{part2(d)}")
