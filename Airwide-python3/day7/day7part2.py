#!/usr/bin/env python3
import re

# Part 2
class Step:
    
    def __init__(self, name):
        self.name = name 
        self.prereqSteps = set()
        self.nextSteps = set()

    def add_nextstep(self, step):
        self.nextSteps.add(step)

    def add_prereqstep(self, step):
        self.prereqSteps.add(step)

    def set_donetime(self, now):
        self.doneTime = now + ord(self.name) - ord('A') + 1 + 60

def get_step(name):
    for step in allSteps:
        if step.name == name:
            return step
    return None

def get_first_avail_step(available, done):
    for stepName in sorted(available):
        step = get_step(stepName)
        if step.prereqSteps.issubset(done):
            return step
            break
    return None

if __name__ == "__main__":

    pattern = r"(?<= )[A-Z](?= )"
    with open("input.txt") as inputFile:
        inputList = [re.findall(pattern, line) for line in inputFile]

    allSteps = []
    availStepNames = set()
    refStepNames = set()
    performedSteps = []
    for rule in inputList:
        for stepName in rule:
            if stepName not in availStepNames and stepName not in refStepNames:
                newStep = Step(stepName)
                allSteps.append(newStep)
        first, second = rule
        for step in allSteps:
            if first == step.name:
                step.add_nextstep(second)
                continue
            if second == step.name:
                step.add_prereqstep(first)
                continue
        if first not in refStepNames:
           availStepNames.add(first)
        refStepNames.add(second)
    
    # loop through available steps
    workers = 5
    idleWorkers = workers
    ongoingSteps = []
    second = 0
    while len(performedSteps) < len(allSteps):
        for step in ongoingSteps:
            if step.doneTime == second:
                availStepNames = availStepNames | step.nextSteps
                performedSteps.append(step.name)
                ongoingSteps.remove(step)
                idleWorkers += 1
        if idleWorkers > 0:
            for i in range(idleWorkers):
                stepToStart = get_first_avail_step(availStepNames, performedSteps)
                if stepToStart == None:
                    break
                stepToStart.set_donetime(second)
                availStepNames.remove(stepToStart.name)
                ongoingSteps.append(stepToStart)
                idleWorkers -= 1
        second += 1
            
    print('Day 7 part 2 result: {} in {} seconds'.format(''.join(performedSteps), second - 1))
