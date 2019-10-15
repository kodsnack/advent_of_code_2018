def solve(infection, immune):
    while infection and immune:
        infection_targeting = []
        immune_targeting = []

        while True:
            earliest = [-1, -1]
            earliest_index = -1
            
            for i, unit in enumerate(infection):
                if unit[-3]:
                    continue

                earliness = [unit[0] * unit[2], unit[3]]
                if earliness > earliest:
                    earliest = earliness
                    earliest_index = i

            if earliest_index == -1:
                break

            unit = infection[earliest_index]
            
            best = [0, 0, 0]
            best_index = -1
            
            for j, target in enumerate(immune):
                if target[-2]:
                    continue

                damage = unit[0] * unit[2]

                if unit[4] in target[5]:
                    damage *= 2
                elif unit[4] in target[6]:
                    damage = 0

                target_attack_power = target[0] * target[2]
                target_initiative = target[3]

                target_package = [damage, target_attack_power, target_initiative]

                if target_package > best:
                    best = target_package
                    best_index = j

            if best_index == -1:
                break

            infection_targeting.append((earliest_index, best_index))

            infection[earliest_index][-3] = True
            immune[best_index][-2] = True

        while True:
            earliest = [-1, -1]
            earliest_index = -1
            
            for i, unit in enumerate(immune):
                if unit[-3]:
                    continue

                earliness = [unit[0] * unit[2], unit[3]]
                if earliness > earliest:
                    earliest = earliness
                    earliest_index = i

            if earliest_index == -1:
                break

            unit = immune[earliest_index]
            
            best = [0, 0, 0]
            best_index = -1
            
            for j, target in enumerate(infection):
                if target[-2]:
                    continue

                damage = unit[0] * unit[2]

                if unit[4] in target[5]:
                    damage *= 2
                elif unit[4] in target[6]:
                    damage = 0

                target_attack_power = target[0] * target[2]
                target_initiative = target[3]

                target_package = [damage, target_attack_power, target_initiative]

                if target_package > best:
                    best = target_package
                    best_index = j

            if best_index == -1:
                break

            immune_targeting.append((earliest_index, best_index))

            immune[earliest_index][-3] = True
            infection[best_index][-2] = True

        infection_attacked = 0
        immune_attacked = 0

        while infection_attacked < len(infection_targeting) or immune_attacked < len(immune_targeting):            
            infection_initiative = -1

            for unit in infection:
                if unit[-1] or not unit[-3]:
                    continue

                if unit[3] > infection_initiative:
                    infection_initiative = unit[3]

            immune_initiative = -1

            for unit in immune:
                if unit[-1] or not unit[-3]:
                    continue

                if unit[3] > immune_initiative:
                    immune_initiative = unit[3]

            if infection_initiative > immune_initiative:
                ai = -1
                attacker = None

                for i, unit in enumerate(infection):
                    if unit[3] == infection_initiative:
                        ai = i
                        attacker = unit
                        break

                defender = None

                for attacki, defendi in infection_targeting:
                    if attacki == ai:
                        defender = immune[defendi]
                        break

                damage = attacker[0] * attacker[2]
                
                if attacker[4] in defender[5]:
                    damage *= 2
                elif attacker[4] in defender[6]:
                    damage = 0

                killing = min(defender[0], damage // defender[1])
                defender[0] -= killing
                attacker[-1] = True

                infection_attacked += 1
            else:
                ai = -1
                attacker = None

                for i, unit in enumerate(immune):
                    if unit[3] == immune_initiative:
                        ai = i
                        attacker = unit
                        break

                defender = None

                for attacki, defendi in immune_targeting:
                    if attacki == ai:
                        defender = infection[defendi]
                        break

                damage = attacker[0] * attacker[2]
                
                if attacker[4] in defender[5]:
                    damage *= 2
                elif attacker[4] in defender[6]:
                    damage = 0

                killing = min(defender[0], damage // defender[1])
                defender[0] -= killing
                attacker[-1] = True

                immune_attacked += 1
        
        immune = [unit for unit in immune if unit[0]]
        infection = [unit for unit in infection if unit[0]]

        for unit in immune:
            unit[-3] = False
            unit[-2] = False
            unit[-1] = False
        for unit in infection:
            unit[-3] = False
            unit[-2] = False
            unit[-1] = False

    return sum(unit[0] for unit in immune) + sum(unit[0] for unit in infection)


def read_and_solve():
    with open('input_24.txt') as f:
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

            unit = [number, hitpoints, damage, initiative, damage_type, weaknesses, immunities, False, False, False]
            
            if in_infection:
                infection.append(unit)
            else:
                immune.append(unit)
                
        return solve(infection, immune)

if __name__ == '__main__':
    print(read_and_solve())