import re


def addr(registers, a, b, c):
    return registers[a] + registers[b]


def addi(registers, a, b, c):
    return registers[a] + b


def mulr(registers, a, b, c):
    return registers[a] * registers[b]


def muli(registers, a, b, c):
    return registers[a] * b


def banr(registers, a, b, c):
    return registers[a] & registers[b]


def bani(registers, a, b, c):
    return registers[a] & b


def borr(registers, a, b, c):
    return registers[a] | registers[b]


def bori(registers, a, b, c):
    return registers[a] | b


def setr(registers, a, b, c):
    return registers[a]


def seti(registers, a, b, c):
    return a


def gtir(registers, a, b, c):
    return 1 if a > registers[b] else 0


def gtri(registers, a, b, c):
    return 1 if registers[a] > b else 0


def gtrr(registers, a, b, c):
    return 1 if registers[a] > registers[b] else 0


def eqir(registers, a, b, c):
    return 1 if a == registers[b] else 0


def eqri(registers, a, b, c):
    return 1 if registers[a] == b else 0


def eqrr(registers, a, b, c):
    return 1 if registers[a] == registers[b] else 0


def solve(examples):
    functions = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]
    
    count = 0

    for example in examples:
        possible = 0
        for function in functions:
            if function(example[0], example[1][1], example[1][2], example[1][3]) == example[2][example[1][3]]:
                possible += 1
        if possible > 2:
            count += 1

    return count


def read_and_solve():
    pattern = re.compile('\d+')
    examples = []
    example = []
    empty_streak = 0

    with open('input_16.txt') as f:
        for line in f:
            if not line.strip():
                empty_streak += 1
                if empty_streak > 1:
                    break
                examples.append(example)
                example = []
                continue
            empty_streak = 0
            example.append([int(val) for val in re.findall(pattern, line) if val])
  
    return solve(examples)

if __name__ == '__main__':
    print(read_and_solve())