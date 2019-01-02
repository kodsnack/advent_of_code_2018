import sys
import re

# Read input
input = sys.stdin.read().strip()
input = input.split('\n')
input = [l.split(' ') for l in input]

# Operations
def addr(A,B,C,registers):
    registers[C] = registers[A] + registers[B]
    return registers
    
def addi(A,B,C,registers):
    registers[C] = registers[A] + B
    return registers

def mulr(A,B,C,registers):
    registers[C] = registers[A] * registers[B]
    return registers

def muli(A,B,C,registers):
    registers[C] = registers[A] * B
    return registers

def banr(A,B,C,registers):
    registers[C] = registers[A] & registers[B]
    return registers

def bani(A,B,C,registers):
    registers[C] = registers[A] & B
    return registers

def borr(A,B,C,registers):
    registers[C] = registers[A] | registers[B]
    return registers

def bori(A,B,C,registers):
    registers[C] = registers[A] | B
    return registers

def setr(A,B,C,registers):
    registers[C] = registers[A]
    return registers

def seti(A,B,C,registers):
    registers[C] = A
    return registers

def gtir(A,B,C,registers):
    if A > registers[B]:
        registers[C] = 1
    else:
        registers[C] = 0
    return registers

def gtri(A,B,C,registers):
    if registers[A] > B:
        registers[C] = 1
    else:
        registers[C] = 0
    return registers

def gtrr(A,B,C,registers):
    if registers[A] > registers[B]:
        registers[C] = 1
    else:
        registers[C] = 0
    return registers

def eqir(A,B,C,registers):
    if A == registers[B]:
        registers[C] = 1
    else:
        registers[C] = 0
    return registers

def eqri(A,B,C,registers):
    if registers[A] == B:
        registers[C] = 1
    else:
        registers[C] = 0
    return registers

def eqrr(A,B,C,registers):
    if registers[A] == registers[B]:
        registers[C] = 1
    else:
        registers[C] = 0
    return registers


oplist = {"addr":addr, "addi":addi, "mulr":mulr, "muli":muli, "banr":banr, "bani":bani, "borr":borr, "bori":bori, "setr":setr, "seti":seti, "gtir":gtir, "gtri":gtri, "gtrr":gtrr, "eqir":eqir, "eqri":eqri, "eqrr":eqrr}

ir = int(input.pop(0)[1])
input = [(o,int(a),int(b),int(c)) for o,a,b,c in input]
ip = 0
registers = [0,0,0,0,0,0]

count = 0
ip = registers[ir]
while ip >= 0 and ip < len(input):
    o,a,b,c = input[ip]
    rb = list(registers)
    registers = oplist[o](a,b,c,list(registers))

    if ip == 28:
        print('Part 1:', max(registers))
        break
    registers[ir] += 1
    ip = registers[ir]


registers = [0,0,0,0,0,0]
count = 0
ip = registers[ir]
seen = set()

done = 0
last = 0
while ip >= 0 and ip < len(input):
    o,a,b,c = input[ip]
    rb = list(registers)

    # Rewrite generator
    if ip == 17:
        # Get comparison register
        o2,a2,b2,c2 = input[ip+3]
        registers[c] = registers[b2] // 256
    else:
        registers = oplist[o](a,b,c,list(registers))


    if ip == 28:
        count += 1
        if registers[a] in seen:
            print('Part 2:', last)
            break
        else:
            seen.add(registers[a])
            last = registers[a]
            
    registers[ir] += 1
    ip = registers[ir]

