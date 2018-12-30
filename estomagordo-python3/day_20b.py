from collections import defaultdict


def solve(d):
    pairs = defaultdict(list)
    pairs[0] = [(0, len(d) - 1)]
    stack = []

    for i, c in enumerate(d):
        if c == '(':
            depth = len(stack) + 1            
            stack.append((depth, i))
        if c == ')':
            depth, pos = stack.pop()
            pairs[depth].append((pos, i))

    replacements = defaultdict(list)

    for depth in range(len(pairs) - 1, -1, -1):
        for start, end in pairs[depth]:
            variants = [[]]

            i = start + 1
            while True:
                if i >= end:
                    break
                c = d[i]

                if i in replacements:              
                    replacement, rend = replacements[i]
                    latest = variants.pop()
                    for r in replacement:
                        variants.append(latest + r)
                    i = rend
                elif c == '|':
                    variants.append([])
                else:
                    variants[-1].append(c)
                i += 1
            replacements[start] = [variants, end]

    distances = {}

    for path in replacements[0][0]:
        x = 0
        y = 0
        p = ''
        for c in path:
            if c == 'E':
                x += 1
            if c == 'S':
                y += 1
            if c == 'W':
                x -= 1
            if c == 'N':
                y -= 1            
            p += c
            if (x, y) not in distances:
                distances[(x, y)] = [len(p), p]
            elif distances[(x, y)][0] > len(p):
                distances[(x, y)] = [len(p), p]

    return len([val for val in distances.values() if val[0] >= 1000])


def read_and_solve():
    with open('input_20.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data[0])

if __name__ == '__main__':
    print(read_and_solve())