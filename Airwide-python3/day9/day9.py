#!/usr/bin/env python3
import re
from blist import blist

with open('input.txt') as f:
    inp = list(map(int, re.findall(r'\d+', f.read())))

circle = blist([0])
num = 1 
score = [0] * inp[0]
curIndex = 0
while num <= inp[1]:
    if num % 23 == 0:
        curIndex = (curIndex - 7 + len(circle)) % len(circle)
        plIndex = (num - 1) % len(score)
        points = num + circle.pop(curIndex)
        score[plIndex] += points
    else:
        curIndex = (curIndex + 2) % len(circle)
        circle.insert(curIndex, num)
    num += 1
print ('Day 9 result:', max(score))
