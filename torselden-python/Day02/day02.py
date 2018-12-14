import os
import sys
import numpy as np

def parse_input(file):
    input = []
    with open(os.path.join(sys.path[0], file)) as input_file:
        for line in input_file:            
            input.append(line)

    return input

def day_2a(input):
    twos = 0
    threes = 0
    for line in input:
        id = list(line)
        numbers, counts= np.unique(id,return_counts = True)

        if 2 in counts:
            twos += 1
        if 3 in counts:
            threes += 1
    
    return twos * threes
    
def correct_id(a,b):
    v= ''
    for i in range(0,len(a)):
        if a[i] == b[i]:
            v+=a[i]
    
    if len(v) == len(a)-1:
        return v
    else:
        return None
    

def day_2b(input):
    for i in range(0,len(input)-1):
        id = input[i]
        for j in range(i+1,len(input)):
            result = correct_id(id, input[j])
            if result:
                return result


print(day_2a(parse_input('2.txt')))
print(day_2b(parse_input('2.txt')))
