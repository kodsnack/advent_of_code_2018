import re


def solve(d):
    pattern = re.compile('\d+')
    y_limits = [10000, -10000]
    clay = set()
    resting = set()
    reached = set()

    for line in d:
        numbers = [int(val) for val in re.findall(pattern, line) if val]
        if line[0] == 'x':
            y_limits[0] = min(y_limits[0], numbers[1])
            y_limits[1] = max(y_limits[1], numbers[2])
            for y in range(numbers[1], numbers[2] + 1):
                clay.add((numbers[0], y))
        else:
            y_limits[0] = min(y_limits[0], numbers[0])
            y_limits[1] = max(y_limits[1], numbers[0])
            for x in range(numbers[1], numbers[2] + 1):
                clay.add((x, numbers[0]))
    
    def is_free(x, y):
        return (x, y) not in clay and (x, y) not in resting

    
    def flow_down(x, y):
        reached.add((x, y))
        print(len(reached))
        print(x, y)
        dy = y
            
        while is_free(x, dy + 1):
            dy += 1
            if dy > y_limits[1]:
                return
            reached.add((x, dy))
        
        left_limit = x
        while is_free(left_limit - 1, dy):
            left_limit -= 1
            if is_free(left_limit, dy + 1):
                break

        right_limit = x
        while is_free(right_limit + 1, dy):
            right_limit += 1
            if is_free(right_limit, dy + 1):
                break
        
        if left_limit == right_limit and dy == y:
            return

        for dx in range(left_limit, right_limit + 1):
            reached.add((dx, dy))
            if not is_free(left_limit, dy + 1) and not is_free(right_limit, dy + 1):
                resting.add((dx, dy))

        if not is_free(left_limit, dy + 1) and not is_free(right_limit, dy + 1):
            flow_down(x, y)
            return

        if is_free(left_limit, dy + 1):
            flow_down(left_limit, dy + 1)
        if is_free(right_limit, dy + 1):
            flow_down(right_limit, dy + 1)

    flow_down(500, max(0, y_limits[0]))
    
    return len(reached)
        

def read_and_solve():
    with open('input_17_small.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())