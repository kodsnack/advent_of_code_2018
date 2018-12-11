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
    
    best = -1000
    best_coord = ''
    
    for x in range(1, 301):
        for y in range(1, 301):
            score = grid[(x, y)]
            if score > best:
                best = score
                best_coord = str(x) + ',' + str(y) + ',1'
            for size in range(2, 301):
                if x + size > 301 or y + size > 301:
                    break
                for dx in range(size):
                    score += grid[(x + dx, y + size - 1)]
                for dy in range(size):
                    score += grid[(x + size - 1, y + dy)]
                score -= grid[(x + size - 1, y + size - 1)]
                if score > best:
                    best = score
                    best_coord = str(x) + ',' + str(y) + ',' + str(size)                

    return best_coord

def read_and_solve():	
    return solve(9424)

if __name__ == '__main__':
    print(read_and_solve())