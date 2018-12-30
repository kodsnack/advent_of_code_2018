#!/usr/bin/env python3
import re
from blist import blist

# with open('input.txt') as f:
#     players, marbles = list(map(int, re.findall(r'\d+', f.read())))

def calcMaxScore(players, marbles):
    circle = blist([0])
    num = 1 
    score = [0] * players
    curIndex = 0
    while num <= marbles:
        if num % 23 == 0:
            curIndex = (curIndex - 7 + len(circle)) % len(circle)
            plIndex = (num - 1) % len(score)
            points = num + circle.pop(curIndex)
            score[plIndex] += points
        else:
            curIndex = (curIndex + 2) % len(circle)
            circle.insert(curIndex, num)
        num += 1
    return max(score)

print ('Day 9 part 1 result:', calcMaxScore(493, 71863))
print ('Day 9 part 2 result:', calcMaxScore(493, 71863 * 100))
