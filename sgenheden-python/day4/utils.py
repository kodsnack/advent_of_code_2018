
import fileinput

import numpy as np
from dateutil.parser import parse as dateparser


def read_stdin():
    data = []
    for line in fileinput.input():
        date, event = line.strip().split("]")
        data.append((dateparser(date[1:]), event.strip()))
    data.sort(key=lambda x: x[0])
    return data


def parse_events(data):
    guard_events = {}
    i = 0
    while i < len(data):
        if data[i][1].find("begins shift") == -1:
            i += 1
            continue
        guard_id = data[i][1].split()[1][1:]
        i += 1
        if guard_id not in guard_events:
            guard_events[guard_id] = []
        while i < len(data) and data[i][1].find("begins shift") == -1:
            event_arr = np.zeros(60, dtype=int)
            event_arr[data[i][0].minute:data[i + 1][0].minute] = 1
            guard_events[guard_id].append(event_arr)
            i += 2
    return guard_events
