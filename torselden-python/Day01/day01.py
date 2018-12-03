import os
import sys

def parse_input():
    input = []
    with open(os.path.join(sys.path[0], '1.txt')) as input_file:
        for line in input_file:
            n = int(line)
            input.append(n)

    return input

def day_1a(input):
    return sum(input)

def day_1b(input):
    freq = 0
    frequencies = {freq:True}
    while (True):
        for line in input:
            freq += line
            if not freq in frequencies:
                frequencies[freq] = True
            else:
                return freq


print(day_1a(parse_input()))
print(day_1b(parse_input()))
