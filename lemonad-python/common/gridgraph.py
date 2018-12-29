"""
Advent of Code Graph helper (Jonas Nockert / @lemonad)

"""
from collections import deque
from enum import Enum, unique
import heapq

import numpy as np


class PriorityQueue:
    def __init__(self):
        self.elements = []
        self.n = 1

    def empty(self):
        return len(self.elements) == 0

    def put(self, item, priority, extra=None):
        """Put a new item on the priority queue with optional extra ordering tuple"""
        if extra is None:
            extra = ()
        # Break ties using entry count n.
        heapq.heappush(self.elements, (priority, *extra, item))
        self.n += 1

    def get(self):
        return heapq.heappop(self.elements)[-1]


class GridGraph:
    def __init__(self):
        self.g = {}

    @classmethod
    def from_chararray(cls, m, skip_symbol="#"):
        graph = cls()

        n_rows, m_cols = np.shape(m)
        for y in range(n_rows):
            for x in range(m_cols):
                node_id = (x, y)
                symbol = m[y, x].decode("utf-8")
                if symbol == skip_symbol:
                    continue
                graph.add_node(node_id, extra={"symbol": symbol})
                north_id = (x, y - 1)
                if graph.get_node(north_id):
                    graph.add_edge(node_id, north_id)
                west_id = (x - 1, y)
                if graph.get_node(west_id):
                    graph.add_edge(node_id, west_id)
        return graph

    def print(self):
        print(self.g)

    def node_ids(self):
        for k in self.g:
            yield k

    def get_node(self, node_id):
        if node_id not in self.g:
            return None
        return self.g[node_id]

    def get_adjacent(self, node_id):
        if node_id not in self.g:
            print("not in", self.g)
            return []
        return self.g[node_id]["adj"]

    def add_node(self, node_id=None, extra=None):
        if node_id is None:
            node_id = max(self.g.keys()) + 1
        self.g[node_id] = {"adj": set(), "weights": {}}
        if extra:
            self.g[node_id].update(extra)
        return node_id

    def add_edge(self, src_node_id, dest_node_id, directed=False, weight=1):
        self.g[src_node_id]["adj"].add(dest_node_id)
        self.g[src_node_id]["weights"][dest_node_id] = weight
        if not directed:
            self.g[dest_node_id]["adj"].add(src_node_id)
            self.g[dest_node_id]["weights"][src_node_id] = weight

    def dims(self):
        min_x = 0
        max_x = 0
        min_y = 0
        max_y = 0
        for k in self.g.keys():
            min_x = min(min_x, self.g[k]["pos"][0])
            max_x = max(max_x, self.g[k]["pos"][0])
            min_y = min(min_y, self.g[k]["pos"][1])
            max_y = max(max_y, self.g[k]["pos"][1])
        return (min_x, max_x, min_y, max_y)

    def all_shortest_paths(self, source_node_id):
        for node_id in sorted(self.g.keys(), reverse=True):
            if node_id == source_node_id:
                continue
            pathlen = self.shortest_path(source_node_id, node_id)
            yield pathlen

    def shortest_path(self, start, goal, just_dists=True):
        if just_dists and "dist" in self.g[goal]:
            return self.g[goal]["dist"]

        frontier = PriorityQueue()
        frontier.put(start, 0)
        came_from = {"start": None}
        cost_so_far = {}
        cost_so_far[start] = 0

        while not frontier.empty():
            current = frontier.get()

            if just_dists:
                if (
                    "dist" in self.g[current]
                    and cost_so_far[current] != self.g[current]["dist"]
                ):
                    raise Exception("Unexpected distance")
                self.g[current]["dist"] = cost_so_far[current]

            if current == goal:
                break

            for next in self.g[current]["adj"]:
                new_cost = cost_so_far[current] + self.g[current]["weights"][next]
                if next not in cost_so_far or new_cost < cost_so_far[next]:
                    cost_so_far[next] = new_cost
                    priority = new_cost
                    frontier.put(next, priority)
                    if not just_dists:
                        came_from[next] = current

        if just_dists:
            return cost_so_far[goal]
        else:
            return cost_so_far[goal], came_from

    def a_star(self, start, goal, heuristic, adjacent=None, priority_extra=None):
        """A* algorithm with customizations.

        E.g. Manhattan heuristics:
            def heuristic_func(a, b, G):
                x1 = a[0]
                y1 = a[1]
                x2 = b[0]
                y2 = b[1]
                return abs(x1 - x2) + abs(y1 - y2)

        E.g. make priority queue take y, x coordinates into account beside cost:
            def extra_func(node_id, G):
                return (node_id[1], node_id[0])

        E.g. Filter adjacent squares
            def adjacent_func(adjacent_ids, goal_id, G):
                adjacent = []
                for adj_id in adjacent_ids:
                    adj = G.get_node(adj_id)
                    if adj['monster'] is not None:
                        continue
                    adjacent.append(adj_id)
                return adjacent
        """
        frontier = PriorityQueue()
        if priority_extra:
            extra = priority_extra(start, self)
        else:
            extra = None
        frontier.put(start, 0, extra=extra)
        came_from = {start: []}
        cost_so_far = {start: 0}

        while not frontier.empty():
            current = frontier.get()
            if current == goal:
                break

            adjacent_ids = self.g[current]["adj"]
            if adjacent:
                adjacent_ids = adjacent(adjacent_ids, goal, self)
            for adj in adjacent_ids:
                new_cost = cost_so_far[current] + 1
                if adj not in cost_so_far or new_cost < cost_so_far[adj]:
                    cost_so_far[adj] = new_cost
                    priority = new_cost + heuristic(goal, adj, self)
                    if priority_extra:
                        extra = priority_extra(adj, self)
                    else:
                        extra = None
                    frontier.put(adj, priority, extra=extra)
                    came_from[adj] = current
        return cost_so_far, came_from
