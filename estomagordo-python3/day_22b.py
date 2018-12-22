from heapq import heappop, heappush


def change_tool(old_terr, new_terr, old_tool):
    if old_terr == new_terr:
        return old_tool
    
    if new_terr == 0:
        return old_terr

    if new_terr == 1:
        if old_terr == 0:
            return 1
        return 0
        
    if old_terr == 0:
        return 2

    return 0


def solve(depth, target_x, target_y):
    margin = 20
    erosions = {}

    for x in range(target_x + margin):
        for y in range(target_y + margin):
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

    frontier = [(0, 0, 0, 2)]
    seen = set()
    
    while frontier:
        time, x, y, tool = heappop(frontier)
        
        if (x, y, tool) in seen:
            continue

        seen.add((x, y, tool))

        if x == target_x and y == target_y:
            return time

        terrain = erosions[(x, y)] % 3

        if (x + 1, y) in erosions:
            new_terr = erosions[(x + 1, y)] % 3
            new_tool = change_tool(terrain, new_terr, tool)
            switching_time = 7 if new_tool != tool else 0
            if x + 1 == target_x and y == target_y and new_tool != 2:
                switching_time += 7
            if (x + 1, y, new_tool) not in seen:
                heappush(frontier, (time + 1 + switching_time, x + 1, y, new_tool))
        if (x, y + 1) in erosions:
            new_terr = erosions[(x, y + 1)] % 3
            new_tool = change_tool(terrain, new_terr, tool)
            switching_time = 7 if new_tool != tool else 0
            if x == target_x and y + 1 == target_y and new_tool != 2:
                switching_time += 7
            if (x, y + 1, new_tool) not in seen:
                heappush(frontier, (time + 1 + switching_time, x, y + 1, new_tool))
        if x > 0:
            new_terr = erosions[(x - 1, y)] % 3
            new_tool = change_tool(terrain, new_terr, tool)
            switching_time = 7 if new_tool != tool else 0
            if x - 1 == target_x and y == target_y and new_tool != 2:
                switching_time += 7
            if (x - 1, y, new_tool) not in seen:
                heappush(frontier, (time + 1 + switching_time, x - 1, y, new_tool))
        if y > 0:
            new_terr = erosions[(x, y - 1)] % 3
            new_tool = change_tool(terrain, new_terr, tool)
            switching_time = 7 if new_tool != tool else 0
            if x == target_x and y - 1 == target_y and new_tool != 2:
                switching_time += 7
            if (x, y - 1, new_tool) not in seen:
                heappush(frontier, (time + 1 + switching_time, x, y - 1, new_tool))
            

def read_and_solve():
    with open('input_22.txt') as f:
        depth = int(f.readline().split()[1])
        target_x, target_y = list(map(int, f.readline().split()[1].split(',')))
        return solve(depth, target_x, target_y)

if __name__ == '__main__':
    print(read_and_solve())