
from utils import read_input


def find_start(nodes, edges, traversed, ongoing):
    candidates = []
    for node in nodes:
        if node in traversed or node in ongoing:
            continue
        last = False
        for edge in edges:
            if edge[1] == node and edge[0] not in traversed:
                last = True
                break
        if not last:
            candidates.append(node)
    if len(candidates) == 0:
        return None
    candidates.sort()
    ongoing.append(candidates[0])
    return ongoing[-1]


if __name__ == "__main__":
    nodes, edges = read_input()
    traversed = []
    ongoing = []
    workers = 5
    offset = 60
    tasks = [None]*workers
    nsec = 0
    while len(traversed) < len(nodes):

        # Load new tasks if available
        for i in range(workers):
            if tasks[i] is None or tasks[i][1] == 0:
                ret = find_start(nodes, edges, traversed, ongoing)
                if ret is not None:
                    tasks[i] = [ret, ord(ret)-ord('A')+1+offset]
                else:
                    tasks[i] = None

        # Do one unit of work
        nsec += 1
        for i in range(workers):
            if tasks[i] is not None:
                tasks[i][1] -= 1
                if tasks[i][1] == 0:
                    traversed.append(tasks[i][0])
    print("".join(traversed))
    print(nsec)
