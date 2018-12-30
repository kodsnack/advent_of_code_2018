from copy import deepcopy
from collections import namedtuple
from collections import defaultdict


def eqrr(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = int(reg[a] == reg[b])
    return reg


def eqri(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = int(reg[a] == b)
    return reg


def eqir(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = int(a == reg[b])
    return reg


def gtrr(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = int(reg[a] > reg[b])
    return reg


def gtri(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = int(reg[a] > b)
    return reg


def gtir(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = int(a > reg[b])
    return reg


def seti(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = a
    return reg


def setr(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = init[a]
    return reg


def bori(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = init[a] | b
    return reg


def borr(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = init[a] | init[b]
    return reg


def bani(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = init[a] & b
    return reg


def banr(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = init[a] & init[b]
    return reg


def muli(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = init[a] * b
    return reg


def mulr(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = init[a] * init[b]
    return reg


def addi(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = init[a] + b
    return reg


def addr(init, a, b, c):
    reg = deepcopy(init)
    reg[c] = init[a] + init[b]
    return reg


def part2(test_cases, inputs):
    opcodes = {
        eqrr: defaultdict(int),
        eqri: defaultdict(int),
        eqir: defaultdict(int),
        gtrr: defaultdict(int),
        gtri: defaultdict(int),
        gtir: defaultdict(int),
        seti: defaultdict(int),
        setr: defaultdict(int),
        bori: defaultdict(int),
        borr: defaultdict(int),
        bani: defaultdict(int),
        banr: defaultdict(int),
        muli: defaultdict(int),
        mulr: defaultdict(int),
        addi: defaultdict(int),
        addr: defaultdict(int)
    }
    for test_case in test_cases:
        for opcode in opcodes.keys():
            ret = opcode(test_case.init, test_case.input[1],
                         test_case.input[2], test_case.input[3])
            if ret == test_case.result:
                opcodes[opcode].update({
                    test_case.input[0]:
                    opcodes[opcode][test_case.input[0]] + 1
                })
    known_ops = defaultdict(lambda x: x)
    while len(known_ops) != 16:
        elem = list(filter(lambda x: len(x[1]) == 1, opcodes.items()))[0]
        key = tuple(elem[1].keys())[0]
        for k, v in opcodes.items():
            v.pop(key, None)
            pass
        known_ops.update({key: elem[0]})
    reg = [0] * 4
    for inp in inputs:
        lst = list(map(int, inp.split()))
        reg = known_ops[lst[0]](reg, lst[1], lst[2], lst[3])
    return reg[0]


def part1(test_cases):
    opcodes = [
        eqrr, eqri, eqir, gtrr, gtri, gtir, seti, setr, bori, borr, bani, banr,
        muli, mulr, addi, addr
    ]
    num = 0
    for test_case in test_cases:
        eq = 0
        for opcode in opcodes:
            ret = opcode(test_case.init, test_case.input[1],
                         test_case.input[2], test_case.input[3])
            if ret == test_case.result:
                eq += 1
        if eq >= 3:
            num += 1
    return num


def solve():
    with open("input.txt") as f:
        TestCase = namedtuple("TestCase", ["init", "input", "result"])
        lines = list(
            filter(
                None,
                map(lambda x: x.strip(),
                    f.read().split("\n\n\n")[0].split("\n"))))
        test_cases = []
        for i in range(0, len(lines), 3):
            init = eval(lines[i].split(":")[1].strip())
            result = eval(lines[i + 2].split(":")[1].strip())
            inp = []
            for sym in lines[i + 1].split():
                inp.append(int(sym))
            test_cases.append(TestCase(init, inp, result))
        f.seek(0)
        inputs = list(
            filter(
                None,
                map(lambda x: x.strip(),
                    f.read().split("\n\n\n")[1].split("\n"))))
        print(f"Answer for the first part:\n{part1(test_cases)}")
        print(f"Answer for the second part:\n{part2(test_cases, inputs)}")


if __name__ == "__main__":
    solve()
