#/usr/bin/python
# AoC2018 day1 A
############################################################
import numpy as np
cur_state = 0

with open('data/day1') as f:
  for line in f:

    c = line[1:-1]
    if line[0] == '+':
      cur_state += int(c)
    if line[0] =='-':
      cur_state-= int(c)
print('Last state is: ' + str(cur_state))

# AoC2018 day1 B
############################################################
import numpy as np

cur_state = 0
states = set()
found = False

while not(found):
  with open('data/day1') as f:
    for line in f:

      c = line[1:-1]
      if line[0] == '+':
        cur_state += int(c)
      if line[0] =='-':
        cur_state-= int(c)
        
      

      if cur_state in states:
        print(cur_state)
        found = True
        break
      states.add(cur_state)
