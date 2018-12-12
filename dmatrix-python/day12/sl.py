from copy import deepcopy


def mutate(initial_state, current_state, instructions, index):
    fn = initial_state.get
    current_pattern = \
        f"{fn(index-2, '.')}{fn(index-1, '.')}{fn(index, '.')}{fn(index+1, '.')}{fn(index+2, '.')}"
    _instruction = instructions.get(current_pattern, None)
    if _instruction is not None:
        current_state.update({index: _instruction})
        return
    if fn(index, None) is None:
        return
    current_state.update({index: "."})


def part2(initial_state: dict, instructions):
    current_state = deepcopy(initial_state)
    last_sum = 0
    for i in range(2000):
        last_sum = sum([k for k, v in initial_state.items() if v == "#"])
        for index in initial_state:
            mutate(initial_state, current_state, instructions, index)
        mx = max(initial_state.keys())
        for index in range(mx, mx + 3):
            mutate(initial_state, current_state, instructions, index)
        initial_state = deepcopy(current_state)
    current_sum = sum([k for k, v in initial_state.items() if v == "#"])
    return (50000000000 - 2000) * (current_sum - last_sum) + current_sum


def part1(initial_state: dict, instructions):
    current_state = deepcopy(initial_state)
    for i in range(20):
        for index in initial_state:
            mutate(initial_state, current_state, instructions, index)
        mx = max(initial_state.keys())
        for index in range(mx, mx + 3):
            mutate(initial_state, current_state, instructions, index)
        initial_state = deepcopy(current_state)
    return sum([k for k, v in initial_state.items() if v == "#"])


def parse(lines):
    initial_state = {k: v for k, v in enumerate(list(lines[0].split()[2]))}
    for i in range(-2, 0):
        initial_state.update({i: "."})
    instructions = {}
    for i in range(2, len(lines)):
        pattern = lines[i].split()[0]
        result = lines[i].split()[2]
        instructions.update({pattern: result})
    return initial_state, instructions


def solve():
    with open("input.txt") as f:
        initial_state, instructions = parse(
            list(map(lambda x: x.strip(), f.readlines())))
        print(
            f"Answer for the first part: \n{part1(initial_state, instructions)}"
        )
        print(
            f"Answer for the second part: \n{part2(initial_state, instructions)}"
        )


if __name__ == '__main__':
    solve()
