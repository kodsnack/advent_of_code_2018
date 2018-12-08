

from utils import read_tree


def sum_meta(node):
    summa = sum(node['meta'])
    for child in node['children']:
        summa += sum_meta(child)
    return summa


if __name__ == "__main__":
    tree = read_tree()
    print(f"Sum of meta = {sum_meta(tree)}")
