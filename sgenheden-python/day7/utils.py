
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
