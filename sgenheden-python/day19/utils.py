
import fileinput

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
    pointer = int(lines[0].strip().split()[1])
    instr = []
    for line in lines[1:]:
        col = line.strip().split()
        instr.append((col[0], [-1]+list(map(int, col[1:]))))
    return pointer, instr

