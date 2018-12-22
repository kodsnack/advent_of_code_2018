def solve(depth, target_x, target_y):
    total = 0
    erosions = {}

    for x in range(target_x + 1):
        for y in range(target_y + 1):
            geo = 0
            if x == target_x and y == target_y:
                geo = 0
            elif y == 0:
                geo = x * 16807
            elif x == 0:
                geo = y * 48271
            else:
                geo = erosions[(x - 1, y)] * erosions[(x, y - 1)]
            erosion = (geo + depth) % 20183
            erosions[(x, y)] = erosion
            total += erosion % 3
    
    return total


def read_and_solve():
    with open('input_22.txt') as f:
        depth = int(f.readline().split()[1])
        target_x, target_y = list(map(int, f.readline().split()[1].split(',')))
        return solve(depth, target_x, target_y)

if __name__ == '__main__':
    print(read_and_solve())