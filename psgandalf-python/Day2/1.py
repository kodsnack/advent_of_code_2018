import re

twos = 0
threes = 0
twos_found = False
threes_found = False

file = open('input.txt', 'r')
input = file.readlines()
for lines in input:
    row = re.sub('\s+', ' ', lines)
    for pos in row:
        if row.count(pos) == 2:
            twos_found = True
        if row.count(pos) == 3:
            threes_found = True
    if twos_found:
        twos += 1
    if threes_found:
        threes += 1
    twos_found = False
    threes_found = False
print(twos*threes)
