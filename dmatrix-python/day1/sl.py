from itertools import cycle

# Part 1
base_var = 0
with open('input.txt') as f:
    my_lines = f.readlines()
    for line in my_lines:
        base_var = eval(f"base_var{line}")
    print(f"Part1 answer: {base_var}")

# Part 2
base_var = 0
set_var = {0}
with open('input.txt') as f:
    my_lines = f.readlines()
    pool = cycle(my_lines)
    for line in pool:
        length = len(set_var)
        base_var = eval(f"base_var{line}")
        set_var.add(base_var)
        if len(set_var) == length:
            print(f"Part2 answer: {base_var}")
            break
