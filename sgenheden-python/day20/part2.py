
import networkx as nx


if __name__ == "__main__":

    nw = nx.read_gpickle("graph.pickle")
    lengths = nx.shortest_path_length(nw, (0,0))
    ans = sum(1 for length in lengths.values() if length >= 1000)
    print(f"Answer is {ans}")
