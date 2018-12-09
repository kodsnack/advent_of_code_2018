def score(graph, node):
    children, metas = graph[node]
    
    if not children:
        return sum(metas)

    return sum(0 if meta > len(children) else score(graph, children[meta - 1]) for meta in metas)


def solve(d):
    graph = { 1: [[], []] }
    node_count = 1
    l = len(d)
    stack = [[1, d[0], d[1]]]

    x = 2

    while x < l:
        node, child_count, meta_count = stack.pop()

        if child_count == 0:
            for _ in range(meta_count):
                graph[node][1].append(d[x])
                x += 1
            continue
            
        child_count -= 1
        node_count += 1
        graph[node_count] = [[], []]
        graph[node][0].append(node_count)
        
        stack.append([node, child_count, meta_count])
        child_count = d[x]
        meta_count = d[x + 1]
        stack.append([node_count, child_count, meta_count])

        x += 2

    return score(graph, 1)

def read_and_solve():
	with open('input_8.txt') as f:
		data = [list(map(int, line.split())) for line in f]
		return solve(data[0])

if __name__ == '__main__':
	print(read_and_solve())