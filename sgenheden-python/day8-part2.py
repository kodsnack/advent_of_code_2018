

from day8_utils import read_tree


def node_value(node):
    if node['children']:
        value = 0
        for idx in node['meta']:
            try:
                value += node_value(node['children'][idx-1])
            except IndexError:
                pass
        return value
    else:
        return sum(node['meta'])


if __name__ == "__main__":
    tree = read_tree()
    print(f"Value of root = {node_value(tree)}")
