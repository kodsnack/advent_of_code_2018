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
    sum=0
    for line in input:
        sum += line
    print(sum)


def day_1b(input):
    sum = 0
    frequencies = {}
    while (True):
        for line in input:
            sum += line
            freq = str(sum)
            if not freq in frequencies:
                frequencies[freq] = True
            else:
                print(freq)
                exit()


day_1a(parse_input())
day_1b(parse_input())
