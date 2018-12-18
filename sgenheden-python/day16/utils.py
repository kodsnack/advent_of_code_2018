

import fileinput
import re


def addr(reg, op):
    reg = list(reg)
    reg[op[3]] = reg[op[1]] + reg[op[2]]
    return reg


def  addi(reg, op):
    reg = list(reg)
    reg[op[3]] = reg[op[1]] + op[2]
    return reg


def mulr(reg, op):
    reg = list(reg)
    reg[op[3]] = reg[op[1]] * reg[op[2]]
    return reg


def  muli(reg, op):
    reg = list(reg)
    reg[op[3]] = reg[op[1]] * op[2]
    return reg

def banr(reg, op):
    reg = list(reg)
    reg[op[3]] = reg[op[1]] & reg[op[2]]
    return reg


def  bani(reg, op):
    reg = list(reg)
    reg[op[3]] = reg[op[1]] & op[2]
    return reg


def borr(reg, op):
    reg = list(reg)
    reg[op[3]] = reg[op[1]] | reg[op[2]]
    return reg


def  bori(reg, op):
    reg = list(reg)
    reg[op[3]] = reg[op[1]] | op[2]
    return reg


def setr(reg, op):
    reg = list(reg)
    reg[op[3]] = reg[op[1]]
    return reg


def  seti(reg, op):
    reg = list(reg)
    reg[op[3]] = op[1]
    return reg

def gtir(reg, op):
    reg = list(reg)
    reg[op[3]] = 1 if op[1] > reg[op[2]] else 0
    return reg


def gtri(reg, op):
    reg = list(reg)
    reg[op[3]] = 1 if reg[op[1]] > op[2] else 0
    return reg

def gtrr(reg, op):
    reg = list(reg)
    reg[op[3]] = 1 if reg[op[1]] > reg[op[2]] else 0
    return reg


def eqir(reg, op):
    reg = list(reg)
    reg[op[3]] = 1 if op[1] == reg[op[2]] else 0
    return reg


def eqri(reg, op):
    reg = list(reg)
    reg[op[3]] = 1 if reg[op[1]] == op[2] else 0
    return reg


def eqrr(reg, op):
    reg = list(reg)
    reg[op[3]] = 1 if reg[op[1]] == reg[op[2]] else 0
    return reg


operations = {
    "addr": addr,
    "addi": addi,
    "mulr": mulr,
    "muli": muli,
    "banr": banr,
    "bani": bani,
    "borr": borr,
    "bori": bori,
    "setr": setr,
    "seti": seti,
    "gtir": gtir,
    "gtri": gtri,
    "gtrr": gtrr,
    "eqir": eqir,
    "eqri": eqri,
    "eqrr": eqrr
}


def read_input():
    lines = [line.strip() for line in fileinput.input()]

    i=0
    befores = []
    instructions = []
    afters = []
    while i < len(lines):
        if lines[i].startswith("Before:"):
            m = re.search("\[.+\]", lines[i])
            befores.append(list(map(int, m.group(0)[1:-1].split(', '))))
            instructions.append(list(map(int, lines[i+1].split())))
            m = re.search("\[.+\]", lines[i+2])
            afters.append(list(map(int, m.group(0)[1:-1].split(', '))))
            i += 3
        else:
            i += 1

    i = len(lines)-1
    while not lines[i].startswith("After:"):
        i -= 1
    i += 4
    examples = []
    for j in range(i, len(lines)):
        examples.append(list(map(int, lines[j].split())))

    return befores, instructions, afters, examples
