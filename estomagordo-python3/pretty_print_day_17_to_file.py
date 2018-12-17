import re

def print_state(clay, resting, reached):
    min_x = 10000
    max_x = -10000
    min_y = 10000
    max_y = -10000

    for collection in (resting, reached):
        for x, y in collection:
            min_x = min(min_x, x)
            max_x = max(max_x, x)
            min_y = min(min_y, y)
            max_y = max(max_y, y)

    with open('17_display.txt', 'w') as f:    
        for y in range(min_y - 1, max_y + 1):
            line = []
            for x in range(min_x - 1, max_x + 1):
                if x == 500 and y == 0:
                    line.append('+')
                elif (x, y) in clay:
                    line.append('#')
                elif (x, y) in resting:
                    line.append('~')
                elif (x, y) in reached:
                    line.append('|')
                else:
                    line.append('.')
            f.write(''.join(line))
            f.write('\n')
    
    print(min_x, max_x, min_y, max_y)

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
    
    starts = set([(500, max(1, y_limits[0]))])

    def is_free(x, y):
        return (x, y) not in clay and (x, y) not in resting

    
    def flow_down(x, y):
        reached.add((x, y))
        dy = y
        new_starts = set()
            
        while is_free(x, dy + 1):
            dy += 1
            if dy > y_limits[1]:
                return new_starts
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
            return new_starts

        for dx in range(left_limit, right_limit + 1):
            reached.add((dx, dy))
            if not is_free(left_limit, dy + 1) and not is_free(right_limit, dy + 1):
                resting.add((dx, dy))

        if not is_free(left_limit, dy + 1) and not is_free(right_limit, dy + 1):
            new_starts.add((x, y))
            return new_starts

        if is_free(left_limit, dy + 1):
            new_starts.add((left_limit, dy))
        if is_free(right_limit, dy + 1):
            new_starts.add((right_limit, dy))

        return new_starts

    while True:
        number = len(reached)
        new_starts = set(starts)
        for x, y in starts:
            new_starts |= flow_down(x, y)
        starts = new_starts
        if len(reached) == number:
            break
    
    print_state(clay, resting, reached)
        

def read_and_solve():
    with open('input_17.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())
