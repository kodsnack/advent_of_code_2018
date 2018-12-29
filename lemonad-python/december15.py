"""
December 15, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import re

import numpy as np

from common.puzzlesolver import PuzzleSolver
from common.gridgraph import GridGraph


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)
        np.set_printoptions(threshold=np.nan)
        self.formatter = lambda x: "%s" % x

    def print_grid(self, G, H, units, annotate_positions=None, annotate_symbol=None):
        """Prints map squares non-numpy style"""
        a_str = self.grid_as_str(
            G,
            H,
            units,
            annotate_positions=annotate_positions,
            annotate_symbol=annotate_symbol,
        )
        print(a_str)

    def grid_as_str(self, G, H, units, annotate_positions=None, annotate_symbol=None):
        """Map squares as non-numpy style string"""
        grid = H.copy()
        for u in units:
            x = units[u]["loc"][0]
            y = units[u]["loc"][1]
            if units[u]["type"] == "Goblin":
                grid[y, x] = b"G"
            else:
                grid[y, x] = b"E"

        if annotate_positions and annotate_symbol:
            for i, pos in enumerate(annotate_positions):
                if len(annotate_symbol) > 1:
                    sym = annotate_symbol[i]
                else:
                    sym = annotate_symbol
                grid[pos[1], pos[0]] = sym

        a_str = np.array2string(
            np.char.decode(grid),
            separator="",
            prefix="",
            suffix="",
            max_line_width=np.nan,
            formatter={"str_kind": self.formatter},
        )
        a_str = a_str.replace("[", "")
        a_str = a_str.replace("]", "")
        a_str = a_str.replace(" ", "")
        return a_str

    @staticmethod
    def extra_func(node_id, G):
        """Make priority queue in A* take reading order into account"""
        return (node_id[1], node_id[0])

    @staticmethod
    def heuristic_func(a, b, G):
        """Heuristic function for A* algorithm"""
        x1 = a[0]
        y1 = a[1]
        x2 = b[0]
        y2 = b[1]
        return abs(x1 - x2) + abs(y1 - y2)

    @staticmethod
    def adjacent_func(adjacent_ids, goal_id, G):
        """Skip occupied squares in A* unless it is the goal"""
        adjacent_ids = sorted(adjacent_ids, key=lambda k: (k[1], k[0]))
        adjacent = []
        for adj_id in adjacent_ids:
            adj = G.get_node(adj_id)
            if "contains" in adj and adj["contains"] is not None and adj_id != goal_id:
                continue
            adjacent.append(adj_id)
        return adjacent

    @staticmethod
    def unit_by_loc(loc, units):
        for u in units:
            if units[u]["loc"] == loc:
                return units[u]
        return None

    @staticmethod
    def units_by_reading_order(units):
        """Return unit IDs sorted by reading order (y, x)"""
        return sorted(units, key=lambda k: (units[k]["loc"][1], units[k]["loc"][0]))

    @staticmethod
    def targets_for_unit(unit_id, units):
        """Return all potential target IDs for a unit"""
        return [k for k in units.keys() if units[k]["type"] != units[unit_id]["type"]]

    @staticmethod
    def targets_by_hp(targets, units):
        """Return target IDs sorted by HP with ties broken by reading order"""
        return sorted(
            targets,
            key=lambda k: (units[k]["HP"], units[k]["loc"][1], units[k]["loc"][0]),
        )

    @staticmethod
    def targets_in_range(unit_id, targets, G, units):
        """Return target units (IDs) adjacent to a unit"""
        in_range = []
        adj = G.get_adjacent(units[unit_id]["loc"])
        for t in targets:
            if units[t]["loc"] in adj:
                in_range.append(t)
        return in_range

    @staticmethod
    def game_over(units):
        types = set()
        for u in units:
            types.add(units[u]["type"])
        return len(types) == 1

    @staticmethod
    def find_adjacent_open_squares(targets, G, units):
        """Return open squares next to target units (in target order)"""
        open_locs = []
        for t in targets:
            for adj in G.get_adjacent(units[t]["loc"]):
                node = G.get_node(adj)
                if "contains" not in node or node["contains"] is None:
                    open_locs.append(adj)
        return open_locs

    def graph_from_input(self, elf_attack=3):
        """Generate graph and numpy matrix from input"""
        all_nodes = []
        node_ids = {}
        node_coords = {}
        units = {}
        H = self.as_char_numpy_array()
        G = GridGraph.from_chararray(H)

        m_id = 0
        for y, row in enumerate(H):
            for x, h in enumerate(row):
                if h != b"G" and h != b"E":
                    continue
                units[m_id] = {"loc": (x, y), "HP": 200, "Attack": 3}
                if h == b"G":
                    units[m_id].update({"type": "Goblin"})
                    H[y, x] = b"."
                elif h == b"E":
                    units[m_id].update({"type": "Elf", "Attack": elf_attack})
                    H[y, x] = b"."
                node = G.get_node((x, y))
                node["contains"] = m_id
                node["symbol"] = "."
                m_id += 1
        return G, H, units

    def fight(self, G, H, units, exit_early=False, last_round=500):
        """Fight! Fight!"""
        rounds = 1
        while rounds <= last_round:
            # Units take turn by reading order.
            unit_ids = self.units_by_reading_order(units)
            for unit_id in unit_ids:
                if unit_id not in units:
                    # Unit killed earlier during round.
                    continue

                # Find all potential targets a unit.
                target_ids = self.targets_for_unit(unit_id, units)
                # No enemies left? Game over.
                if not target_ids:
                    total_hp = 0
                    for uid in units:
                        total_hp += units[uid]["HP"]
                    return (rounds - 1) * total_hp, units, self.grid_as_str(G, H, units)

                #
                # Move phase.
                #

                targets_in_range = self.targets_in_range(unit_id, target_ids, G, units)
                # Only move if not already adjacent to a target.
                if not targets_in_range:
                    # Find target adjacent squares
                    open_squares = self.find_adjacent_open_squares(target_ids, G, units)
                    paths = {}
                    loc = units[unit_id]["loc"]
                    for open_loc in open_squares:
                        cost_so_far, came_from = G.a_star(
                            open_loc,
                            loc,
                            self.heuristic_func,
                            adjacent=self.adjacent_func,
                            priority_extra=self.extra_func,
                        )
                        if loc not in cost_so_far:
                            # No path to square.
                            continue
                        paths[open_loc] = {
                            "cost": cost_so_far[loc],
                            "dest": open_loc,
                            "path": came_from,
                            "costs": cost_so_far,
                        }
                    if not paths:
                        continue

                    sorted_paths = sorted(
                        paths,
                        key=lambda k: (
                            paths[k]["cost"],
                            paths[k]["path"][loc][1],
                            paths[k]["path"][loc][0],
                        ),
                    )
                    if sorted_paths:
                        path_id = sorted_paths[0]
                        next_loc = paths[path_id]["path"][loc]
                        next_node = G.get_node(next_loc)
                        node = G.get_node(loc)
                        next_node["contains"] = node["contains"]
                        node["contains"] = None
                        units[unit_id]["loc"] = next_loc

                #
                # Attack phase.
                #

                targets_in_range = self.targets_in_range(unit_id, target_ids, G, units)
                if targets_in_range:
                    by_hp = self.targets_by_hp(targets_in_range, units)
                    opponent = by_hp[0]
                    units[opponent]["HP"] -= units[unit_id]["Attack"]
                    if units[opponent]["HP"] <= 0:
                        if exit_early and units[opponent]["type"] == "Elf":
                            return None, units, self.grid_as_str(G, H, units)
                        node = G.get_node(units[opponent]["loc"])
                        node["contains"] = None
                        del units[opponent]

            rounds += 1

        # Reached round limit without win.
        return None, units, self.grid_as_str(G, H, units)

    def solve_part_one(self, last_round=500):
        """Solution for part one."""
        G, H, units = self.graph_from_input()
        return self.fight(G, H, units, last_round=last_round)

    def solve_part_two(self, last_round=500):
        """Solution for part two."""
        for a in range(4, 50):
            G, H, units = self.graph_from_input(elf_attack=a)
            outcome, units, grid_str = self.fight(
                G, H, units, last_round=last_round, exit_early=True
            )
            if outcome:
                return outcome, units, grid_str
        # Reached elf attack upper limit.
        return None, units, grid_str

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december15.input")
    one, units, grid_str = s.solve_part_one()
    print("Combat outcome:", one)
    two, units, grid_str = s.solve_part_two()
    print("Combat outcome with elf attack boost:", two)
