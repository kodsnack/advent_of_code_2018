
import fileinput


def read_tree():
    input_str = "".join(line.strip() for line in fileinput.input())
    spec = list(map(int, input_str.split()))
    tree = parse_node(spec)
    return tree


def parse_node(spec):
    nchildren = spec.pop(0)
    nmeta = spec.pop(0)
    children = [parse_node(spec) for _ in range(nchildren)]
    meta = [spec.pop(0) for _ in range(nmeta)]
    return {"meta": meta, "children": children}
