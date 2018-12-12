def calculate_score(pots, side_stretch):
    val = -side_stretch
    total = 0

    for pot in pots:
        if pot == '#':
            total += val
        val += 1

    return total


def solve(d):
    generations = 20
    side_stretch = 50

    pots = ['.'] * side_stretch + list(d[0].split()[-1]) + ['.'] * side_stretch
    pot_count = len(pots)

    rules = {}

    for line in d[2:]:
        state, _, result = line.split()
        rules[state] = result

    for _ in range(generations):
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

    return calculate_score(pots, side_stretch)


def read_and_solve():
    with open('input_12.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())