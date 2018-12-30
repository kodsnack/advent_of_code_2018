from aocbase import readInput
import re

inp = readInput()
p = re.compile(r"^Step (.) must be finished before step (.) can begin\.$")

def lineParse(s, f=lambda x:x):
    m = p.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x):
    return list(map(lambda x, fx=ff:f(x, fx), inp.splitlines()))

def nodeDependencies(rel):
    r = dict()
    s = set()
    for c1, c2 in rel:
        if c1 not in r:
            r[c1] = set()
        if c2 not in r:
            r[c2] = set()
        r[c2].add(c1)
    return r

def allNodes(rel):
    res = set()
    for c1, c2 in rel:
        res.add(c1)
        res.add(c2)
    return res

def nodeReady(node, rel, finishedNodes):
    for depNode in rel[node]:
        if depNode not in finishedNodes:
            return False
    return True

def nodeOrder(rel, theNodes):
    order = list()
    finishedNodes = set()
    while finishedNodes != theNodes:
        sortedNodes = sorted(list(theNodes-finishedNodes))
        for node in sortedNodes:
            if nodeReady(node, rel, finishedNodes):
                finishedNodes.add(node)
                order.append(node)
                break
    return ''.join(order)

def calculateWork(theNodes):
    res = dict()
    for node in theNodes:
        res[node] = ord(node) - ord('A') + 61
    return res

def nodeParallellWorkTime(rel, theNodes, nofWorkers):
    finishedNodes = set()
    work = calculateWork(theNodes)
    totalTime = 0
    while finishedNodes != theNodes:
        sortedNodes = sorted(list(theNodes - finishedNodes))
        workers = nofWorkers
        tasks = list()
        for node in sortedNodes:
            if nodeReady(node, rel, finishedNodes):
                tasks.append(node)
                workers -= 1
                if workers == 0:
                    break
        for task in tasks:
            work[task] -= 1
            if work[task] == 0:
                finishedNodes.add(task)
        totalTime += 1
    return totalTime

relations = fileParse(inp, ff=str)
dep = nodeDependencies(relations)
print("Solution to day 7 part 1:", nodeOrder(dep, allNodes(relations)))
print("Solution to day 7 part 2:", nodeParallellWorkTime(dep, allNodes(relations), 5))
