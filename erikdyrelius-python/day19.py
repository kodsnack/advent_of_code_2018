from aocbase import readInput
import re
from collections import deque

inp = readInput()
p = re.compile(r"(\w+) (\d+) (\d+) (\d+)")
inp2 = '''#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5'''


def doAddr(inst, reg):
    reg[inst[3]] = reg[inst[1]] + reg[inst[2]]

def doAddi(inst, reg):
    reg[inst[3]] = reg[inst[1]] + inst[2]

def doMulr(inst, reg):
    reg[inst[3]] = reg[inst[1]] * reg[inst[2]]

def doMuli(inst, reg):
    reg[inst[3]] = reg[inst[1]] * inst[2]

def doBanr(inst, reg):
    reg[inst[3]] = reg[inst[1]] & reg[inst[2]]

def doBani(inst, reg):
    reg[inst[3]] = reg[inst[1]] & inst[2]

def doBorr(inst, reg):
    reg[inst[3]] = reg[inst[1]] | reg[inst[2]]

def doBori(inst, reg):
    reg[inst[3]] = reg[inst[1]] | inst[2]

def doSetr(inst, reg):
    reg[inst[3]] = reg[inst[1]]

def doSeti(inst, reg):
    reg[inst[3]] = inst[1]

def doGtir(inst, reg):
    if inst[1] > reg[inst[2]]:
        reg[inst[3]] = 1
    else:
        reg[inst[3]] = 0

def doGtri(inst, reg):
    if reg[inst[1]] > inst[2]:
        reg[inst[3]] = 1
    else:
        reg[inst[3]] = 0

def doGtrr(inst, reg):
    if reg[inst[1]] > reg[inst[2]]:
        reg[inst[3]] = 1
    else:
        reg[inst[3]] = 0

def doEqir(inst, reg):
    if inst[1] == reg[inst[2]]:
        reg[inst[3]] = 1
    else:
        reg[inst[3]] = 0

def doEqri(inst, reg):
    if reg[inst[1]] == inst[2]:
        reg[inst[3]] = 1
    else:
        reg[inst[3]] = 0

def doEqrr(inst, reg):
    if reg[inst[1]] == reg[inst[2]]:
        reg[inst[3]] = 1
    else:
        reg[inst[3]] = 0

d = {
    "addr":doAddr,
    "addi":doAddi,
    "mulr":doMulr,
    "muli":doMuli,
    "banr":doBanr,
    "bani":doBani,
    "borr":doBorr,
    "bori":doBori,
    "setr":doSetr,
    "seti":doSeti,
    "gtir":doGtir,
    "gtri":doGtri,
    "gtrr":doGtrr,
    "eqir":doEqir,
    "eqri":doEqri,
    "eqrr":doEqrr
}

def parse(s):
    l = []
    for line in s.splitlines():
        m = p.match(line)
        if m:
            l.append([d[m.group(1)]] + list(map(int, m.groups()[1:])))
        else:
            if line[0]=='#':
                ip = int(line[4])
            else:
                print("Error:"+line)
    return l, ip

def run(ip, prog, reg=[0]*6):
    while True:
        instr = prog[reg[ip]]
        print(reg, instr)
        instr[0](instr, reg)
        reg[ip] += 1
        if reg[ip] < 0 or reg[ip] >= len(prog):
            break
    return reg[0]


instr, ip = parse(inp)
print(run(ip, instr))
print(run(ip, instr, [1, 0, 0, 0, 0, 0]))

print("Solution to day 19 part 1:",19.1)
print("Solution to day 19 part 2:",19.2)
