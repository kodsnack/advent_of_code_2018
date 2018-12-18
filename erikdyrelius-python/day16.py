from aocbase import readInput
import re

inp = readInput()
p = re.compile(r"-?\d+")

def parse(s):
    tc = list()
    prog = []
    for line in s.splitlines():
        tokens = list(line.split())
        if len(tokens)==0:
            pass
        elif tokens[0] == "Before:":
            tc.append(dict())
            tc[-1]["pre"]= (int(tokens[1][1:-1]), int(tokens[2][:-1]),
                            int(tokens[3][:-1]), int(tokens[4][:-1]))
        elif tokens[0] == "After:":
            tc[-1]["post"]= (int(tokens[1][1:-1]), int(tokens[2][:-1]),
                            int(tokens[3][:-1]), int(tokens[4][:-1]))
        else:
            inst = list(map(int, tokens))
            if len(tc)>0 and "post" not in tc[-1]:
                tc[-1]["instr"] = inst
            else:
                prog.append(inst)
    return tc, prog

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

cmds = [doAddr, doAddi, doMulr, doMuli, doBanr, doBani,
        doBorr, doBori, doSetr, doSeti, doGtir, doGtri,
        doGtrr, doEqir, doEqri, doEqrr]

def doCmd(inst, reg, cmds):
    cmds[inst[0]](inst, reg)

def testCmds(tc):
    smp = 0
    tryCmds = dict()
    for t in tc:
        inst = t['instr']
        sm = 0
        for c in cmds:
            reg = list(t['pre'])
            tryCmds[inst[0]] = c
            doCmd(inst, reg, tryCmds)
            if tuple(reg) == t['post']:
                sm += 1
        if sm >= 3:
            smp+=1
    return smp

def findCmds(tc):
    rc ={}
    tryCmds = dict()
    candidates = []
    known = set()
    for i in range(16):
        candidates.append(set(cmds))
    while len(known)<len(cmds):
        for t in tc:
            inst = t['instr']
            failed = set()
            for c in candidates[inst[0]]:
                reg = list(t['pre'])
                tryCmds[inst[0]] = c
                doCmd(inst, reg, tryCmds)
                if tuple(reg) != t['post']:
                    failed.add(c)
            candidates[inst[0]] = candidates[inst[0]] - failed
            if candidates[inst[0]] == 0:
                print("Error no candidates for:", inst[0])
            elif len(candidates[inst[0]]) == 1:
                theCmd = list(candidates[inst[0]])[0]
                known.add(theCmd)
                for i in range(16):
                    if theCmd in candidates[i] and len(candidates[i])>1:
                        candidates[i].remove(theCmd)
    for i in range(16):
        rc[i] = list(candidates[i])[0]
    return rc

def runCmds(rc, prog):
    reg = [0]*4
    for st in prog:
        doCmd(st, reg, rc)
    return reg[0]

tc, prog = parse(inp)
print("Solution to day 16 part 1:",testCmds(tc))
rc = findCmds(tc)
print("Solution to day 16 part 2:",runCmds(rc, prog))
