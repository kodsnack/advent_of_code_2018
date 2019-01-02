import sys
import re

# Read input
input = sys.stdin.read().strip()

input = input.split('\n')
input = [l.split(' ') for l in input]

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

# Run program
ip = registers[ir]
while ip >= 0 and ip < len(input):
    o,a,b,c = input[ip]
    rb = list(registers)
    registers = oplist[o](a,b,c,list(registers))
    registers[ir] += 1
    ip = registers[ir]

print('Part 1:', registers[0])


# The program finds the sum of divisors of a large number
# Run program enough loops to generate the number
ip = 0
registers = [1,0,0,0,0,0]
ip = registers[ir]
for i in range(100):
    o,a,b,c = input[ip]
    rb = list(registers)
    registers = oplist[o](a,b,c,list(registers))
    registers[ir] += 1
    ip = registers[ir]

# Extract number
n = max(registers)

# Find sum of divisors
ds = []
for i in range(1,n+1):
    if n % i == 0:
       ds.append(i)
print('Part 2:', sum(ds))
