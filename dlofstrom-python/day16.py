import sys
import re

# Read input
input = sys.stdin.read().strip()
input = input.split('\n\n')
# Samples come in sets of three
samples = [s.split('\n') for s in input[:-2]]
samples = [[list(map(int, re.findall('-?\d+',l))) for l in s] for s in samples]
# Example program
test_program = input[-1].split('\n')
test_program = [list(map(int, re.findall('-?\d+',l))) for l in test_program]

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


oplist = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

# Initially every opcode can be any instruction
opcodes = {opcode:set(oplist) for opcode in set(o for o,a,b,c in [h[1] for h in samples])}
three_or_more = 0
for before,instruction,after in samples:
    opcode, a, b, c = instruction
    # Save possible instructions for opcode
    possible_operations = set(operation for operation in oplist if operation(a,b,c,list(before)) == after)
    # Note if 3 or more possible
    if len(possible_operations) >= 3:
        three_or_more += 1
    # Update possible instructions for opcode
    opcodes[opcode].intersection_update(possible_operations)
print('Part 1:', three_or_more)

# Those who only have one possible instruction can be eliminated from all others
while max(opcodes, key=lambda opcode: len(opcodes[opcode])) > 1:
    # Unique opcode instructions (union of sets)
    unique_operations = set.union(*[operations for operations in opcodes.values() if len(operations) == 1])
    # Not yet decided opcode instructions (list of sets)
    multiple_operations = [operations for operations in opcodes.values() if len(operations) > 1]
    for operations in multiple_operations:
        operations.difference_update(unique_operations)

# Make opcodes functions instead of sets
opcodes = {opcode:operations.pop() for opcode,operations in opcodes.items()}

# Run test program
registers = [0,0,0,0]
for opcode,a,b,c in test_program:
    opcodes[opcode](a,b,c,registers)
print('Part 2:', registers[0])
