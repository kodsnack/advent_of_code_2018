
import fileinput


def read_stdin() :
    items = []
    max_x, max_y = 0, 0
    for line in fileinput.input() :
        item = line.strip().split()
        id = item[0][1:]
        start_x, start_y = map(int, item[2][:-1].split(","))
        len_x, len_y = map(int, item[3].split("x"))
        max_x = max(max_x, start_x + len_x + 1)
        max_y = max(max_y, start_y + len_y + 1)
        items.append((start_x, start_y, start_x + len_x, start_y + len_y, id))
    return items, max_x, max_y
