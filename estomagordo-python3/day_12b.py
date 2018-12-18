def calculate_score(pots, side_stretch):
    val = -side_stretch
    total = 0

    for pot in pots:
        if pot == '#':
            total += val
        val += 1

    return total


def solve(d):
    total_generations = 5 * 10**10
    unchanged_criterion = 10
    side_stretch = 500

    pots = ['.'] * side_stretch + list(d[0].split()[-1]) + ['.'] * side_stretch
    pot_count = len(pots)

    rules = {}

    for line in d[2:]:
        state, _, result = line.split()
        rules[state] = result

    unchanged = [-10**9]
    prev = -10**9
    current_generation = 0
    
    while True:
        current_generation += 1
        new_pots = ['.', '.']

        for x in range(2, pot_count - 2):
            stretch = ''.join(pots[x - 2:x + 3])
            pot = '.'
            
            for rule, result in rules.items():
                if stretch == rule:
                    pot = result
                    break
            
            new_pots.append(pot)

        pots = new_pots

        score = calculate_score(pots, side_stretch)
        diff = score - prev
        prev = score

        if diff == unchanged[0]:
            if len(unchanged) == unchanged_criterion:
                return score + (total_generations - current_generation) * diff
            unchanged.append(diff)
        else:
            unchanged = [diff]

    return calculate_score(pots, side_stretch)


def read_and_solve():
    with open('input_12.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())