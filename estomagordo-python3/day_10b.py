import re

def pretty_print(points, min_x, max_x, min_y, max_y):
    point_set = set()

    for x, y, dx, dy in points:
        point_set.add((x, y))   
        
    lines = []

    for y in range(max_y - min_y + 1):
        lines.append(['-'] * (max_x - min_x + 1))

    for x in range(min_x, max_x + 1):
        for y in range(min_y, max_y + 1):
            if (x, y) in point_set:
                lines[y - min_y][x - min_x] = 'x'

    return '\n'.join(''.join(c for c in line) for line in lines)


def move_points(points):
    for i in range(len(points)):
        x, y, dx, dy = points[i]        
        points[i] = [x + dx, y + dy, dx, dy]
        

def solve(d):
    height = 10
    pattern = re.compile(r'-?\d*')
    points = [[int(val) for val in re.findall(pattern, line) if val] for line in d]
    time = 0

    while True:
        move_points(points)

        time += 1
        
        min_y = min(point[1] for point in points)
        max_y = max(point[1] for point in points)

        if max_y - min_y < height:
            return time

def read_and_solve():
    with open('input_10.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())