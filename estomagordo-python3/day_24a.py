def solve(infection, immune):
    while infection and immune:
        infection.sort(key=lambda unit: [-unit[0] * unit[2], -unit[3]])
        immune.sort(key=lambda unit: [-unit[0] * unit[2], -unit[3]])

        infection_targeted = [False] * len(infection)
        immune_targeted = [False] * len(immune)

        infection_targeting = []
        immune_targeting = []

        for i, unit in enumerate(infection):
            best = [0, 0, 0]
            best_index = -1

            for j, defender in enumerate(immune):
                if immune_targeted[j]:
                    continue

                damage = unit[0] * unit[2]
                
                if unit[4] in defender[5]:
                    damage *= 2
                elif unit[4] in defender[6]:
                    damage = 0

                target_attack_power = defender[0] * defender[2]
                target_initiative = defender[3]

                target_package = [damage, target_attack_power, target_initiative]

                if target_package > best:
                    best = target_package
                    best_index = j

            if best_index >= 0:
                target = immune[best_index][-1]
                infection_targeting.append((i, target))

            immune_targeted[immune[best_index][-1]] = True

        for i, unit in enumerate(immune):
            best = 0
            best_index = -1

            for j, defender in enumerate(infection):
                if infection_targeted[j]:
                    continue

                damage = unit[0] * unit[2]
                
                if unit[4] in defender[5]:
                    damage *= 2
                elif unit[4] in defender[6]:
                    damage = 0

                if damage > best:
                    best = damage
                    best_index = j

            if best_index >= 0:
                target = infection[best_index][-1]
                immune_targeting.append((i, target))

            infection_targeted[infection[best_index][-1]] = True

        infection.sort(key=lambda unit: -unit[3])
        immune.sort(key=lambda unit: -unit[3])

        infection_attacked = 0
        immune_attacked = 0

        while True:
            if infection_attacked == len(infection) and immune_attacked == len(immune):
                break
            
            infection_attacking = True

            if infection_attacked == len(infection_targeting):
                infection_attacking = False
            elif immune_attacked < len(immune_targeting):
                immune_initiative = immune[immune_attacked][3]
                infection_initiative = infection[infection_attacked][3]
                
                if immune_initiative > infection_initiative:
                    infection_attacking = False

            if infection_attacking:
                attacker = infection[infection_attacked]
                ai = attacker[-1]
                di = -1

                for pair in infection_targeting:
                    if pair[0] == ai:
                        di = pair[1]
                        break
                
                defender_index = -1

                for i, def_possible in enumerate(immune):
                    if def_possible[-1] == di:
                        defender_index = i    

                defender = immune[defender_index]

                damage = attacker[0] * attacker[2]
                
                if attacker[4] in defender[5]:
                    damage *= 2
                elif attacker[4] in defender[6]:
                    damage = 0

                killing = min(defender[0], damage // defender[1])
                defender[0] -= killing

                infection_attacked += 1
            else:
                attacker = immune[immune_attacked]
                ai = attacker[-1]
                di = -1

                for pair in immune_targeting:
                    if pair[0] == ai:
                        di = pair[1]
                        break
                
                defender_index = -1

                for i, def_possible in enumerate(immune):
                    if def_possible[-1] == di:
                        defender_index = i    

                defender = infection[defender_index]

                damage = attacker[0] * attacker[2]
                
                if attacker[4] in defender[5]:
                    damage *= 2
                elif attacker[4] in defender[6]:
                    damage = 0

                killing = min(defender[0], damage // defender[1])
                defender[0] -= killing

                immune_attacked += 1
        
        immune = [unit for unit in immune if unit[0]]
        infection = [unit for unit in infection if unit[0]]

    return sum(unit[0] for unit in immune) + sum(unit[0] for unit in infection)


def read_and_solve():
    with open('input_24_small.txt') as f:
        immune = []
        infection = []

        in_infection = False
        
        for line in f:
            if not line.strip():
                in_infection = True
                continue

            if line.strip()[-1] == ':':
                continue

            parts = line.split()
            number = int(parts[0])
            hitpoints = int(parts[4])
            damage = int(parts[-6])
            initiative = int(parts[-1])
            damage_type = parts[-5]

            weaknesses = []
            immunities = []

            in_paren = False
            in_weakness = False
            in_immunities = False

            for part in parts:
                if part == 'weak' or part[1:] == 'weak':
                    in_weakness = True
                    in_immunities = False
                if part == 'immune' or part[1:] == 'immune':
                    in_weakness = False
                    in_immunities = True

                if part[0] == '(':
                    in_paren = True
                    continue                

                if not in_paren or part in ('to', 'weak', 'immune'):
                    continue

                if in_weakness:
                    weaknesses.append(part[:-1])
                elif in_immunities:
                    immunities.append(part[:-1])

                if part[-1] == ')':
                    in_paren = False

            unit = [number, hitpoints, damage, initiative, damage_type, weaknesses, immunities]
            
            if in_infection:
                unit.append(len(infection))
                infection.append(unit)
            else:
                unit.append(len(immune))
                immune.append(unit)
        # print(infection)
        # print(immune)
        return solve(infection, immune)

if __name__ == '__main__':
    print(read_and_solve())