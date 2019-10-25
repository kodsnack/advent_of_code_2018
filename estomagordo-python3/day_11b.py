def solve(d):
    grid = []

    for y in range(301):
        row = []
        for x in range(301):
            rack_id = x + 10
            power_level = y * rack_id
            power_level += d
            power_level *= rack_id
            power_level = (power_level // 100) % 10
            power_level -= 5
            row.append(power_level)
        grid.append(row)
    
    best = -1000
    best_coord = ''

    for x in range(1, 301):
        for y in range(1, 301):
            score = grid[y][x]            
            for step in range(299 - max(x, y)):
                for sx in range(step):
                    score += grid[y + step][x + sx]
                for sy in range(step):
                    score += grid[y + sy][x + step]
                score += grid[y + step][x + step]
                if score > best:
                    best = score
                    best_coord = ','.join(map(str, [x, y, step + 1]))

    return best_coord

def read_and_solve():	
    return solve(9424)

if __name__ == '__main__':
    print(read_and_solve())