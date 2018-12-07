
import fileinput
import re


def read_input() :
    edges = []
    nodes = []
    for line in fileinput.input() :
        m = re.search(r"Step (.) must be finished before step (.) can begin", line)
        edges.append((m.group(1), m.group(2)))
        nodes.append(m.group(1))
        nodes.append(m.group(2))
    return set(nodes), edges


def find_start(nodes, edges, traversed):
    candidates = []
    for node in nodes:
        if node in traversed:
            continue
        last = False
        for edge in edges:
            if edge[1] == node and edge[0] not in traversed:
                last = True
                break
        if not last:
            candidates.append(node)
    candidates.sort()
    traversed.append(candidates[0])
    return traversed[-1]


if __name__ == "__main__":
    nodes, edges = read_input()
    traversed = []
    start = find_start(nodes, edges, traversed)
    while len(traversed) < len(nodes):
        # Find edges starting with start
        candidates = [edge[1] for edge in edges if edge[0] == start]
        # Remove edges with a start that has not been traversed yet
        remove = []
        for edge in edges:
            if edge[0] != start and edge[1] in candidates and edges[0] not in traversed:
                remove.append(edge[1])
        candidates = [c for c in candidates if not c in remove]
        # Traverse an edge
        if len(candidates) > 0:
            candidates.sort()
            traversed.append(candidates[0])
            idx = edges.index((start, candidates[0]))
        start = find_start(nodes, edges, traversed)
    print("".join(traversed))
