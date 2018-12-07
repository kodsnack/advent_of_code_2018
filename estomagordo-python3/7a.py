from collections import defaultdict
from heapq import heappop, heappush


def solve(d):
    graph = defaultdict(set)
    steps = set()

    for line in d:
        s = line.split()
        pre = s[1]
        post = s[7]
        steps.add(pre)
        steps.add(post)
        graph[post].add(pre)

    out = ''
    frontier = []

    for c in steps:
        if c not in graph:
            heappush(frontier, c)

    while frontier:
        c = heappop(frontier)
        out += c
        removing = []

        for k, v in graph.items():
            if c in v:
                if len(v) == 1:
                    removing.append(k)
                v.remove(c)

        for r in removing:
            heappush(frontier, r)

    return out

with open('input_7.txt') as f:
	data = [line.rstrip() for line in f]
	print(solve(data))