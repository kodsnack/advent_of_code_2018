import copy
import sys
import re
from collections import defaultdict

RE_IN = re.compile(r"^Step (.).*step (.).*$")


nodes = set()
graph = defaultdict(list)
with open(sys.argv[1]) as f:
    for line in f:
        m = RE_IN.match(line)
        assert m is not None
        graph[m.group(2)].append(m.group(1))
        nodes.add(m.group(1))
        nodes.add(m.group(2))


def _find_available_nodes(nodes, graph):
    return [n for n in sorted(nodes) if not graph[n]]


def part1(nodes, graph):
    order = []
    while nodes:
        n = _find_available_nodes(nodes, graph)[0]
        order.append(n)
        nodes.remove(n)
        for child, parent in graph.items():
            if n in parent:
                graph[child].remove(n)
    return "".join(order)


print(part1(set(nodes), copy.deepcopy(graph)))

#
# Part 2
WORKERS = 5
DURATION = 60


def part2(nodes, graph):
    second = 0
    start = defaultdict(int)

    working = []
    while nodes or working:
        for n in working:
            if second >= start[n] + DURATION + (ord(n) - ord("A") + 1):
                working.remove(n)
                for ch, parents in graph.items():
                    if n in parents:
                        graph[ch].remove(n)
                nodes.remove(n)

        ns = _find_available_nodes(nodes, graph)
        candidates = set(ns) - set(working) if len(working) < WORKERS else {}
        for n in candidates:
            if n not in working:
                start[n] = second
                working.append(n)

        second += 1

    return second - 1


print(part2(nodes, graph))
