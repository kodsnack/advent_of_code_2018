from aocbase import readInput
import re

inp = readInput()
#: [1518-04-21 00:57] wakes up
#: [1518-09-03 00:12] falls asleep
#: [1518-04-21 00:04] Guard #3331 begins shift
p = re.compile(r"^\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (wakes up|falls asleep|Guard #(\d+) begins shift)$")

def linePar(s, f=lambda x:x):
    m = p.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def filePar(inp, f=linePar, ff=lambda x:x):
    return list(map(lambda x, fx=ff:f(x, fx), inp.splitlines()))

events = filePar(inp)
events.sort()
guardSleepTime = dict()
guardId = "None"
sleepStart = 0
for event in events:
    if event[5][0] == 'G':
        guardId = event[5].split()[1][1:]
    if event[5][0] == 'f':
        sleepStart = int(event[4])
    if event[5][0] == 'w':
        guardSleepTime[guardId] = guardSleepTime.get(guardId, 0) + int(event[4]) - sleepStart
mt = 0
mg = 0
for guard in guardSleepTime:
    if guardSleepTime[guard] > mt:
        mt = guardSleepTime[guard]
        mg = guard
minuteMap = [0]*59
on = False
mxt, mxm = 0, 0
for event in events:
    if event[5][0] == 'G':
        if mg == event[5].split()[1][1:]:
            on = True
        else:
            on = False
    if on:
        pass
    if event[5][0] == 'f':
        sleepStart = int(event[4])
    if event[5][0] == 'w':
        if on == True:
            for j in range(sleepStart, int(event[4])):
                minuteMap[j] += 1
                if minuteMap[j] > mxt:
                    mxt = minuteMap[j]
                    mxm = j
mxt, mxm, mxg = 0, 0, ""
d3 = dict()
for i in events:
    if i[5][0] == 'G':
        gg = i[5].split()[1][1:]
    if i[5][0] == 'f':
        sleepStart = int(i[4])
    if i[5][0] == 'w':
        for j in range(sleepStart, int(i[4])):
            if gg not in d3:
                d3[gg] = [0]*60
            d3[gg][j] += 1
            if d3[gg][j] > mxt:
                mxt = d3[gg][j]
                mxm = j
                mxg = gg
print("Solution to day 4 part 1:", int(mg)*mxm)
print("Solution to day 4 part 2:", mxm*int(mxg))
