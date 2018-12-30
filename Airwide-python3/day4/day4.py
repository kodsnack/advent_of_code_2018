#!/usr/bin/env python3
import re
from operator import itemgetter
from collections import OrderedDict
from datetime import datetime as dt
import numpy as np

def get_minute(dt_string):
    return int(dt_string[-2:])

pattern = r"\d+-\d+-\d+ \d+:\d+|#\d+|falls asleep|wakes up"
with open("input.txt") as input_file:
    input_list = [re.findall(pattern, line) for line in input_file]
input_list = sorted(input_list, key=itemgetter(0))

# Part 1
dates = []
guards = []
shifts = []
for entry in input_list:
    if entry[1][0] == '#':
        dates.append(entry[0])
        guards.append(entry[1])
        shift = [0] * 60
        shifts.append(shift)
    elif entry[1] == 'falls asleep':
        nap_start_minute = get_minute(entry[0])
    elif entry[1] == 'wakes up':
        nap_last_minute = get_minute(entry[0]) 
        for i in range(nap_start_minute, nap_last_minute):
            shift[i] = 1
    else:
        print("Input list is not in correct order")
        break
guard_most_asleep = ""
guard_most_asleep_at_minute = ""
max_total_sleep = 0
max_asleep_at_minute = 0

for guard in set(guards):
    guard_shifts = []
    for i in range(len(shifts)):
        if guards[i] == guard:
            guard_shifts.append(shifts[i])
    total_sleep = sum(map(sum, guard_shifts))
    asleep_per_minute = [sum(i) for i in zip(*guard_shifts)]
    asleep_per_minute_max = max(asleep_per_minute)
    most_sleepy_minute = asleep_per_minute.index(asleep_per_minute_max)
    if total_sleep > max_total_sleep:
        max_total_sleep = total_sleep
        guard_most_asleep = guard
    if asleep_per_minute_max > max_asleep_at_minute:
        max_asleep_at_minute = asleep_per_minute_max 
        guard_most_asleep_at_minute = guard
        total_most_sleepy_minute = most_sleepy_minute 
print("Guard most asleep: {} @ {} minutes.".format(guard_most_asleep, max_total_sleep))
print("Most sleepy minute: {}".format(most_sleepy_minute))
print("Part 1 result: {}".format(int(guard_most_asleep[1:]) * most_sleepy_minute))
print("Guard {} was most asleep at minute {}.".format(guard_most_asleep_at_minute, total_most_sleepy_minute))
print("Part 2 result: {}".format(int(guard_most_asleep_at_minute[1:]) * total_most_sleepy_minute))
    
