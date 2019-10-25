import re


def distance(a, b):
    return sum(abs(a[x] - b[x]) for x in range(3))


def solve(bots):
    radiest = max(bot[3] for bot in bots)
    
    inrange = 0

    for bota in bots:        
        if bota[3] != radiest:
            continue
        for botb in bots:
            if distance(bota, botb) <= bota[3]:
                inrange += 1
    
    return inrange

def read_and_solve():
    pattern = re.compile(r'-?\d+')
    with open('input_23.txt') as f:
        data = []
        for line in f:
            data.append([int(val) for val in re.findall(pattern, line) if val])
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())