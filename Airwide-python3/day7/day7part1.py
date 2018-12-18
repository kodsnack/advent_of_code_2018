#!/usr/bin/env python3
import re



# Part 1

class Step:
    
    def __init__(self, name):
        self.name = name 
        self.prereqSteps = set()
        self.nextSteps = set()

    def add_nextstep(self, step):
        self.nextSteps.add(step)

    def add_prereqstep(self, step):
        self.prereqSteps.add(step)

def get_step(name):
    for step in allSteps:
        if step.name == name:
            return step
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
    while len(performedSteps) < len(allSteps):
        for stepName in sorted(availStepNames):
            currentStep = get_step(stepName)
            print('prereqSteps: {}, performedSteps: {}'.format(currentStep.prereqSteps, performedSteps))
            if currentStep.prereqSteps.issubset(performedSteps):
                availStepNames = availStepNames | currentStep.nextSteps
                availStepNames.remove(stepName)
                performedSteps.append(stepName)
                print(availStepNames,performedSteps)
                break
            
    print('Day 7 part 1 result: {}'.format(''.join(performedSteps)))
         
         
         
         
         
         
