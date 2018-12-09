import sys
import re
from collections import namedtuple, defaultdict
from datetime import datetime

Guard = namedtuple('Guard', ['id_', 'sleep', 'wake'], defaults=[None, None])

IN_RE = re.compile(r'\[(.*)\]\s(.*)$')
GUARD_RE = re.compile(r'Guard #(\d+)')

#
# Parse input
lines = []
with open(sys.argv[1]) as f:
    for line in f:
        m = IN_RE.match(line)
        if m is None:
            raise Exception()

        timestamp, text = m.groups()
        timestamp = datetime.strptime(timestamp, '%Y-%m-%d %H:%M')
        lines.append((timestamp, text))

lines = sorted(lines, key=lambda x: x[0])

guards = []
guard = None
for timestamp, text in lines:
    if text.startswith('Guard'):
        if guard and guard.sleep and guard.wake:
            guards.append(guard)
        id_ = int(GUARD_RE.match(text).group(1))
        guard = Guard(id_)
    elif text.startswith('falls'):
        if guard.sleep is not None:
            guards.append(guard)
        guard = guard._replace(sleep=timestamp, wake=None)
    elif text.startswith('wakes'):
        guard = guard._replace(wake=timestamp)

# Add last guard
guards.append(guard)

#
# Part 1
most_sleep = defaultdict(int)
for g in guards:
    sleep_time = g.wake-g.sleep
    most_sleep[g.id_] += sleep_time.seconds

sleepy_head = max(most_sleep.items(), key=lambda x: x[1])[0]

# Filter out the guard with most asleep time
sleeps = [g for g in guards if g.id_ == sleepy_head]
minutes_most_sleep = defaultdict(int)
for m in range(60):
    for g in sleeps:
        if g.sleep.minute <= m and m < g.wake.minute:
            minutes_most_sleep[m] += 1

minute = max(minutes_most_sleep.items(), key=lambda x: x[1])[0]

print(sleepy_head * minute)

#
# Part 2
gid, minute, most = None, -1, 0
for m in range(60):
    counts = defaultdict(int)
    for g in guards:
        if g.sleep.minute <= m and m < g.wake.minute:
            counts[g.id_] += 1

    if not counts:
        continue

    mx = max(counts.items(), key=lambda x: x[1])
    if mx[1] > most:
        minute = m
        most = mx[1]
        gid = mx[0]

print(gid*minute)
