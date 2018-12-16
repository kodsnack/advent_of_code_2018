import re


def addr(registers, a, b):
    return registers[a] + registers[b]


def addi(registers, a, b):
    return registers[a] + b


def mulr(registers, a, b):
    return registers[a] * registers[b]


def muli(registers, a, b):
    return registers[a] * b


def banr(registers, a, b):
    return registers[a] & registers[b]


def bani(registers, a, b):
    return registers[a] & b


def borr(registers, a, b):
    return registers[a] | registers[b]


def bori(registers, a, b):
    return registers[a] | b


def setr(registers, a, b):
    return registers[a]


def seti(registers, a, b):
    return a


def gtir(registers, a, b):
    return 1 if a > registers[b] else 0


def gtri(registers, a, b):
    return 1 if registers[a] > b else 0


def gtrr(registers, a, b):
    return 1 if registers[a] > registers[b] else 0


def eqir(registers, a, b):
    return 1 if a == registers[b] else 0


def eqri(registers, a, b):
    return 1 if registers[a] == b else 0


def eqrr(registers, a, b):
    return 1 if registers[a] == registers[b] else 0


def solve(examples, program):
    functions = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]
    
    opcodes = []

    for x in range(16):
        opcodes.append(set())

    for opcode in range(16):
        for x, function in enumerate(functions):
            matching = True
            for before, operation, after in examples:
                if operation[0] != opcode:
                    continue
                result = function(before, operation[1], operation[2])
                if result != after[operation[3]]:
                    matching = False
                    break
            if matching:
                opcodes[opcode].add(x)

    isolated = set()
    while any(len(opcode) > 1 for opcode in opcodes):
        for opcode in opcodes:
            if len(opcode) == 1:
                isolated |= opcode
                continue
            opcode -= isolated
    
    registers = [0] * 4

    for x, instruction in enumerate(program):
        print(x)
        code, a, b, c = instruction
        function = functions[list(opcodes[code])[0]]
        value = function(registers, a, b)
        registers[c] = value

    return registers[0]


def read_and_solve():
    pattern = re.compile('\d+')
    examples = []
    example = []
    empty_streak = 0
    program_mode = False
    program = []

    with open('input_16.txt') as f:
        for line in f:
            if program_mode:
                if line.strip():
                    program.append([int(val) for val in re.findall(pattern, line) if val])
                    continue
                continue
            if not line.strip():
                empty_streak += 1
                if empty_streak > 1:
                    program_mode = True
                    continue
                examples.append(example)
                example = []
                continue
            empty_streak = 0
            example.append([int(val) for val in re.findall(pattern, line) if val])
  
    return solve(examples, program)

if __name__ == '__main__':
    print(read_and_solve())