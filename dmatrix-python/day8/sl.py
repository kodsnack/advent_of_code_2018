from typing import List


def cnt(graph: dict, node: int) -> int:
    children, meta = graph[node]
    if not children:
        return sum(meta)

    return sum(0 if meta > len(children) else cnt(graph, children[meta - 1])
               for meta in meta)


def part2(lines: List[int]) -> int:
    gh = {1: [[], []]}
    node_cnt = 1
    ln = len(lines)
    acc = 2
    stack = [[1, lines[0], lines[1]]]
    while acc < ln:
        node, children, meta = stack.pop()

        if children == 0:
            for _ in range(meta):
                gh[node][1].append(lines[acc])
                acc += 1
            continue
        children -= 1
        node_cnt += 1
        gh[node_cnt] = [[], []]
        gh[node][0].append(node_cnt)
        stack.append([node, children, meta])
        stack.append([node_cnt, lines[acc], lines[acc + 1]])
        acc += 2
    return cnt(gh, 1)


def part1(lines: List[int]) -> int:
    ln = len(lines)
    acc = 2
    total = 0
    stack = [[lines[0], lines[1]]]
    while acc < ln:
        children, meta = stack.pop()

        if children == 0:
            for _ in range(meta):
                total += lines[acc]
                acc += 1
            continue

        stack.append([children - 1, meta])
        stack.append([lines[acc], lines[acc + 1]])
        acc += 2
    return total


if __name__ == "__main__":
    with open("input.txt") as f:
        my_lines = list(map(lambda r: int(r.strip()), f.read().split()))
        print(f"Answer for the first part {part1(my_lines)}")
        print(f"Answer for the first part {part2(my_lines)}")
