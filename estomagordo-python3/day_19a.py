def solve(d):
    ipreg = int(d[0][-1])
    ip = 0

    instructions = []

    for line in d[1:]:
        ls = line.split()
        instructions.append([ls[0]] + list(map(int, ls[1:])))

    inscount = len(instructions)
    registers = [0] * 6

    while True:
        if not 0 <= ip < inscount:
            break

        instruction, a, b, c = instructions[ip]

        registers[ipreg] = ip

        if instruction == 'seti':
            registers[c] = a
        elif instruction == 'addi':
            registers[c] = registers[a] + b
        elif instruction == 'setr':
            registers[c] = registers[a]
        elif instruction == 'addr':
            registers[c] = registers[a] + registers[b]
        elif instruction == 'muli':
            registers[c] = registers[a] * b
        elif instruction == 'mulr':
            registers[c] = registers[a] * registers[b]
        elif instruction == 'gtrr':
            registers[c] = 1 if (registers[a] > registers[b]) else 0
        elif instruction == 'eqrr':
            registers[c] = 1 if (registers[a] == registers[b]) else 0

        ip = registers[ipreg]
        ip += 1

    return registers[0]


def read_and_solve():
    with open('input_19.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())