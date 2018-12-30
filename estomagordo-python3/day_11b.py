def solve(d):
    grid = {}

    for x in range(1, 301):
        for y in range(1, 301):
            rack_id = x + 10
            power_level = y * rack_id
            power_level += d
            power_level *= rack_id
            power_level = (power_level // 100) % 10
            power_level -= 5
            grid[(x, y)] = power_level

    rowsums = [[] for _ in range(300)]

    for y in range(1, 301):
        cum = 0
        for x in range(1, 301):
            cum += grid[(x, y)]
            rowsums[y - 1].append(cum)
    
    best = -1000
    best_coord = ''
    
    for size in range(1, 301):
        for x in range(1, 302 - size):
            for y in range(1, 302 - size):
                score = 0
                for dy in range(size):
                    score += rowsums[y + dy - 1][x + size - 2] - rowsums[y + dy - 1][x - 1]
                if score > best:
                    best = score
                    best_coord = ','.join(map(str, [x, y, size]))

    return best_coord

def read_and_solve():	
    return solve(9424)

if __name__ == '__main__':
    print(read_and_solve())