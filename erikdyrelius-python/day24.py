from aocbase import readInput
import re
from collections import deque
from copy import deepcopy

inp = readInput()
inp2 = '''Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4'''
p = re.compile(r"(\d+) units each with (\d+) hit points (\(([^\)]+)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)")

def parseUnit(g):
    d = dict()
    d["units"] = int(g[0])
    d["hp"] = int(g[1])
    d["weak"] = []
    d["immune"] = []
    if g[3] != None:
        for s in g[3].split():
            t = s.strip()
            t = t.strip(",")
            t = t.strip(";")
            if t == 'weak':
                l = d["weak"]
            elif t == 'immune':
                l = d['immune']
            elif t != 'to':
                l.append(t)
    d["damage"] = int(g[4])
    d["type"] = g[5]
    d["initiative"] = int(g[6])
    return d

def parse(s):
    immune = []
    infections = []
    for line in s.splitlines():
        m = p.match(line)
        if m:
            l.append(parseUnit(m.groups()))
            l[-1]["number"] = idx
            l[-1]["side"] = side
            idx += 1
        else:
            if line == "Immune System:":
                l = immune
                idx = 1
                side = "immune"
            elif line == "Infection:":
                l = infections
                idx = 1
                side = "infection"
            elif line != "":
                print("Error:", line)
    return immune, infections

def selectTarget(unit, enemies, taken):
    mxd = 0
    mxp = 0
    mxi = 0
    mxn = 0
    for enemy in enemies:
        if enemy["number"] in taken:
            continue
        if enemy["units"] == 0:
            continue
        dmg = unit["damage"]*unit["units"]
        if unit["type"] in enemy["weak"]:
            dmg *= 2
        if unit["type"] in enemy["immune"]:
            continue
        if dmg < mxd:
            continue
        if dmg == mxd:
            epwr = enemy["damage"] * enemy["units"]
            if epwr < mxp:
                continue
            if epwr == mxp:
                if enemy["initiative"] < mxi:
                    continue
        mxd = dmg
        mxp = enemy["damage"] * enemy["units"]
        mxi = enemy["initiative"]
        mxn = enemy["number"]
    unit["starg"] = mxn
    return mxn

def sortGroup(g):
    g.sort(key=lambda x:(x["units"]*x["damage"], x["initiative"]), reverse=True)

def targetSelection(infs, imms):
    tinf, timm = dict(), dict()
    taken = set()
    for inf in infs:
        i = inf["number"]
        t = selectTarget(inf, imms, taken)
        tinf[i] = t
        taken.add(t)
    taken = set()
    for imm in imms:
        i = imm["number"]
        t = selectTarget(imm, infs, taken)
        timm[i] = t
        taken.add(t)

def doAttacks(inf, imm):
    tot = sorted(inf + imm, key=lambda x:x["initiative"], reverse=True)
    for unit in tot:
        for enemy in tot:
            if unit["starg"] == enemy["number"] and unit["side"] != enemy["side"]:
                dmg = unit["damage"]*unit["units"]
                if unit["type"] in enemy["weak"]:
                    dmg *= 2
                if unit["type"] in enemy["immune"]:
                    dmg = 0
                dmg //= enemy["hp"]
                enemy["units"] = max(0, enemy["units"]-dmg)

def countAlive(side):
    cnt = 0
    for unit in side:
        cnt += unit["units"]
    return cnt

def addBoost(l, boost):
    for unit in l:
        unit["damage"] += boost

def fightToTheDeath(imm, inf, boost=0):
    addBoost(imm, boost)
    oldim, oldin = 0, 0
    while True:
        sortGroup(inf)
        sortGroup(imm)
        targetSelection(inf, imm)
        doAttacks(inf, imm)
        if countAlive(inf) == 0 or countAlive(imm) == 0:
            break
        if oldim==countAlive(imm) and oldin==countAlive(inf):
            break
        oldim, oldin = countAlive(imm), countAlive(inf)
    return imm, inf

def findRightBoost(immo, info):
    boost = 0
    while True:
        imm, inf = fightToTheDeath(deepcopy(immo), deepcopy(info), boost)
        if countAlive(inf) == 0:
            break
        boost += 1
    return countAlive(inf)+countAlive(imm)

immo, info = parse(inp)
imm, inf = fightToTheDeath(deepcopy(immo), deepcopy(info))
print("Solution to day 24 part 1:",countAlive(inf)+countAlive(imm))
print("Solution to day 24 part 2:",findRightBoost(immo, info))
