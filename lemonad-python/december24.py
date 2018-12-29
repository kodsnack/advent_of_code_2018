"""
December 24, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from enum import Enum
import re

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    class GroupType(Enum):
        IMMUNE = 1
        INFECTION = 2

    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def effective_power(group):
        return group["damage"] * group["n"]

    @staticmethod
    def damage(att_g, def_g):
        if att_g["type"] == def_g["type"]:
            return 0
        damage = Solver.effective_power(att_g)
        att_type = att_g["dtype"]
        if att_type in def_g["immune"]:
            return 0
        elif att_type in def_g["weak"]:
            return damage * 2
        else:
            return damage

    @staticmethod
    def units_killed(g, damage):
        return damage // g["hp"]

    @staticmethod
    def fight(groups, boost=0):
        for g in groups:
            if g["type"] == Solver.GroupType.IMMUNE:
                g["damage"] += boost

        while True:
            group_ids = list(range(len(groups)))

            # Target selection phase
            sel_order = sorted(
                group_ids,
                key=lambda i: (Solver.effective_power(groups[i]), groups[i]["init"]),
                reverse=True,
            )
            att = []
            sel = []
            for so in sel_order:
                dmg = [
                    (
                        Solver.damage(groups[so], groups[i]),
                        Solver.effective_power(groups[i]),
                        groups[i]["init"],
                        i,
                    )
                    for i in group_ids
                    if (groups[i]["type"] != groups[so]["type"] and i not in sel)
                ]
                dmg_order = sorted(dmg, reverse=True)
                if dmg_order and dmg_order[0][0] != 0:
                    att.append((so, dmg_order[0][3]))
                    sel.append(dmg_order[0][3])

            # Attacking phase
            att_order = sorted(att, key=lambda i: (groups[i[0]]["init"]), reverse=True)

            any_kills = False
            for ai, di in att_order:
                if groups[ai]["n"] <= 0 or groups[di]["n"] <= 0:
                    continue
                damage = Solver.damage(groups[ai], groups[di])
                n_killed = Solver.units_killed(groups[di], damage)
                if n_killed:
                    groups[di]["n"] = max(0, groups[di]["n"] - n_killed)
                    any_kills = True
            if not any_kills:
                # fight stuck because e.g. both remaining groups immune against
                # each other attacks or damage is less than one unit killed.
                return None, None

            groups = [g for g in groups if g["n"] > 0]
            imm = [
                g["n"]
                for g in groups
                if (g["type"] == Solver.GroupType.IMMUNE and g["n"] > 0)
            ]
            inf = [
                g["n"]
                for g in groups
                if (g["type"] == Solver.GroupType.INFECTION and g["n"] > 0)
            ]
            if len(imm) <= 0:
                return sum(inf), Solver.GroupType.INFECTION
            elif len(inf) <= 0:
                return sum(imm), Solver.GroupType.IMMUNE

    def parse_groups(self):
        groups = []

        reading_immune = False
        reading_infection = False
        for line in self.lines():
            if re.search("Immune System", line):
                reading_immune = True
                reading_infection = False
                continue
            elif re.search("Infection", line):
                reading_immune = False
                reading_infection = True
                continue
            elif not line:
                continue

            m = re.search(
                r"(\d+) units each with (\d+) hit points "
                "(\(.+\) )*with an attack that "
                "does (\d+) (\w+) damage at initiative (\d+)",
                line,
            )
            if not m:
                raise Exception("No match")

            n_units = int(m.group(1))
            n_hp = int(m.group(2))
            damage = int(m.group(4))
            damage_type = m.group(5)
            initiative = int(m.group(6))
            weak_immune_comb = m.group(3)

            immune = []
            weak = []
            if weak_immune_comb:
                weak_immune_list = weak_immune_comb.split(";")
                for weak_immune in weak_immune_list:
                    m = re.search(r"immune to ([^\)]+)", weak_immune)
                    if m:
                        immune = m.group(1).split(", ")

                    m = re.search(r"weak to ([^\)]+)", weak_immune)
                    if m:
                        weak = m.group(1).split(", ")

            if reading_immune:
                group_type = self.GroupType.IMMUNE
            elif reading_infection:
                group_type = self.GroupType.INFECTION
            else:
                raise Exception("No reading")

            groups.append(
                {
                    "type": group_type,
                    "n": n_units,
                    "hp": n_hp,
                    "damage": damage,
                    "dtype": damage_type,
                    "init": initiative,
                    "immune": immune,
                    "weak": weak,
                }
            )
        return groups

    def solve_part_one(self):
        """Solution for part one."""
        groups = self.parse_groups()
        units_left, who_won = self.fight(groups)
        return units_left

    def solve_part_two(self, boost=0):
        """Solution for part two."""
        # TODO Binary search
        i = 0
        while True:
            groups = self.parse_groups()
            units_left, who_won = self.fight(groups, boost=i)
            if who_won == self.GroupType.IMMUNE:
                return units_left
            i += 1

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december24.input")
    (one, two) = s.solve()
    print("units left of winning army:", one)
    print("units left for immune system:", two)
