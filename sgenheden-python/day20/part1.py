
import fileinput
import networkx as nx


def move(direction, current):
    if direction == 'N':
        return current[0], current[1]-1
    elif direction == 'S':
        return current[0], current[1] + 1
    elif direction == 'W':
        return current[0] - 1, current[1]
    elif direction == 'E':
        return current[0] + 1, current[1]
    else:
        raise Exception(f"Unknown direction {dir}")


def parse_input2(network, inp, current):
    inp = inp[1:]  # Remove first parenthesis
    tail = current
    while True:
        if inp[0] == "(":
            inp, current = parse_input2(network, inp, current)
        elif inp[0] == "|":
            current = tail
        elif inp[0] == ")":
            break
        else:
            new = move(inp[0], current)
            if new not in network.nodes:
                network.add_node(new)
            network.add_edge(current, new)
            current = new
        inp = inp[1:]
    return inp, current


if __name__ == "__main__":

    inp = "".join(line.strip() for line in fileinput.input())[1:-1]
    inp = "(" + inp + ")"
    nw = nx.Graph()
    nw.add_node((0,0))
    parse_input2(nw, inp, (0,0))
    nx.write_gpickle(nw, "graph.pickle")
    lengths = nx.shortest_path_length(nw, (0, 0))
    print(f"Maximum doors are {max(lengths.values())}")
