inp = "513401"

recipieScores = [3, 7]
elfRecipies = [0, 1]

def newRecipies(recipieScores, elf1, elf2):
    newSum = recipieScores[elf1] + recipieScores[elf2]
    if newSum >= 10:
        recipieScores.append(1)
        recipieScores.append(newSum-10)
    else:
        recipieScores.append(newSum)
    nofRecipies = len(recipieScores)
    elf1 += (recipieScores[elf1] + 1)
    while elf1 >= nofRecipies:
        elf1 -= nofRecipies
    elf2 += (recipieScores[elf2] + 1)
    while elf2 >= nofRecipies:
        elf2 -= nofRecipies
    return elf1, elf2

def compareEnd(recipieScores, pattern):
    patternLength = len(pattern)
    if recipieScores[-patternLength:] == pattern:
        return len(recipieScores) - inpLength
    if recipieScores[-patternLength-1:-1] == pattern:
        return len(recipieScores) - patternLength - 1
    return False

def findTenAfter(nofRecipies, startingRecipies):
    recipieScores = startingRecipies[:]
    nofRecipies = int(nofRecipies)
    elf1, elf2 = 0, 1
    while len(recipieScores) < 10+nofRecipies:
        elf1, elf2 = newRecipies(recipieScores, elf1, elf2)
    return ''.join(map(str, recipieScores[nofRecipies:nofRecipies+10]))

def numberOfRecipiesBefore(pattern, startingRecipies):
    pattern = list(map(int, pattern))
    recipieScores = startingRecipies[:]
    elf1, elf2 = 0, 1
    while not compareEnd(recipieScores, pattern):
        elf1, elf2 = newRecipies(recipieScores, elf1, elf2)
    return compareEnd(recipieScores, pattern)

print("Solution to day 14 part 1: {}".format(findTenAfter(inp, recipieScores)))
print("Solution to day 14 part 2: {}".format(numberOfRecipiesBefore(inp, recipieScores)))