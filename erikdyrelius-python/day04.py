from aocbase import readInput
import re

inp = readInput()
#: [1518-04-21 00:57] wakes up
#: [1518-09-03 00:12] falls asleep
#: [1518-04-21 00:04] Guard #3331 begins shift
p = re.compile(r"^\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (wakes up|falls asleep|Guard #(\d+) begins shift)$")

def lineParse(s, f=lambda x:x):
    m = p.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x):
    return list(map(lambda x, fx=ff:f(x, fx), inp.splitlines()))

def shiftStart(event):
    return event[5][0] == 'G'

def fallAsleep(event):
    return event[5][0] == 'f'

def wakeUp(event):
    return event[5][0] == 'w'

def getGuardId(event):
    return event[6]

def getMinute(event):
    return int(event[4])

def calcGuardSleepTime(events):
    guardSleepTime = dict()
    guardId = "None"
    sleepStart = 0
    for event in events:
        if shiftStart(event):
            guardId = getGuardId(event)
        if fallAsleep(event):
            sleepStart = getMinute(event)
        if wakeUp(event):
            guardSleepTime[guardId] = guardSleepTime.get(guardId, 0) + getMinute(event) - sleepStart
    return guardSleepTime

def findSleepiestGuard(guardSleepTime):
    maxTime = 0
    theGuard = 0
    for guard in guardSleepTime:
        if guardSleepTime[guard] > maxTime:
            maxTime = guardSleepTime[guard]
            theGuard = guard
    return theGuard

def findSleepiestTimeForGuard(guard, guardSleepTime):
    minuteMap = [0]*59
    sleeping = False
    maxTime, theMinute = 0, 0
    for event in events:
        if shiftStart(event):
            if guard == getGuardId(event):
                sleeping = True
            else:
                sleeping = False
        elif fallAsleep(event):
            sleepStart = getMinute(event)
        if wakeUp(event):
            if sleeping == True:
                for minute in range(sleepStart, getMinute(event)):
                    minuteMap[minute] += 1
                    if minuteMap[minute] > maxTime:
                        maxTime = minuteMap[minute]
                        theMinute = minute
    return theMinute

def findSleepiestTime(guardSleepTime):
    maxTime, theMinute, theGuard = 0, 0, ""
    minuteMaps = dict()
    for event in events:
        if shiftStart(event):
            guard = getGuardId(event)
        if fallAsleep(event):
            sleepStart = getMinute(event)
        if wakeUp(event):
            for minute in range(sleepStart, getMinute(event)):
                if guard not in minuteMaps:
                    minuteMaps[guard] = [0]*60
                minuteMaps[guard][minute] += 1
                if minuteMaps[guard][minute] > maxTime:
                    maxTime = minuteMaps[guard][minute]
                    theMinute = minute
                    theGuard = guard
    return theMinute, theGuard


events = fileParse(inp)
events.sort()

guardSleepTime = calcGuardSleepTime(events)
sleepiestGuard = findSleepiestGuard(guardSleepTime)
sleepiestTime = findSleepiestTimeForGuard(sleepiestGuard, guardSleepTime)

print("Solution to day 4 part 1:", int(sleepiestGuard)*sleepiestTime)

theMinute, theGuard = findSleepiestTime(guardSleepTime)


print("Solution to day 4 part 2:", theMinute*int(theGuard))
