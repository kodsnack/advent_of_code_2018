#!/usr/bin/env python3
import string
def reacted_polymer(polymer):
    not_done = True
    while not_done:
        for i in range(len(polymer) - 1):
            if polymer[i].upper() == polymer[i+1].upper():
                if polymer[i] != polymer[i+1]:
                    polymer = polymer[:i] + polymer[i+2:]
                    break
            if i == len(polymer) - 2:
                not_done = False
    return polymer

with open("input.txt") as input_file:
    polymer = input_file.read().strip()

# Part 1
polymer = reacted_polymer(polymer)
print("Day 5 part 1 result: {}".format(len(polymer)))

# Part 2
units = string.ascii_lowercase
min_polymer_len = 50000
for unit in units:
    mod_pol = polymer[:]
    mod_pol = mod_pol.replace(unit, "")
    mod_pol = mod_pol.replace(unit.upper(), "")
    reacted_len = len(reacted_polymer(mod_pol))
    if reacted_len < min_polymer_len:
        min_polymer_len = reacted_len

print("Day 5 part 2 result: {}".format(min_polymer_len))
