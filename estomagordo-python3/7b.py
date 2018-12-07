from collections import defaultdict
from heapq import heappop, heappush


def assign_workers(workers, frontier, base, added_time):
    while frontier:
        if all(worker[0] for worker in workers):
            break
        c = heappop(frontier)
        time = ord(c) - base + added_time
        for i, worker in enumerate(workers):
            if not worker[0]:
                workers[i] = [c, time]
                break


def solve(d, number_workers, added_time):
    base = ord('A')
    graph = defaultdict(set)
    steps = set()

    for line in d:
        s = line.split()
        pre = s[1]
        post = s[7]
        steps.add(pre)
        steps.add(post)
        graph[post].add(pre)
        
    frontier = []
    total_time = 0
    workers = [['', -1] for _ in range(number_workers)]

    for c in steps:
        if c not in graph:
            heappush(frontier, c)

    assign_workers(workers, frontier, base, added_time)

    while True:
        if not any(worker[0] for worker in workers):
            break
        total_time += 1
        done = []
        done_indices = set()

        for i, worker in enumerate(workers):
            c, t = worker
            if t == 1:
                done.append(c)
                done_indices.add(i)

        for i in range(len(workers)):
            if i in done_indices:
                workers[i] = ['', -1]
            else:
                workers[i][1] -= 1

        removing = []

        for k, v in graph.items():
            for c in done:
                if c in v:
                    if len(v) == 1:
                        removing.append(k)
                    v.remove(c)

        for r in removing:
            heappush(frontier, r)

        assign_workers(workers, frontier, base, added_time)

    return total_time

with open('input_7.txt') as f:
	data = [line.rstrip() for line in f]
	print(solve(data, 5, 61))