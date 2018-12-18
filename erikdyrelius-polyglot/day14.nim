# def newRecipies(recipieScores, elfRecipies):
#     recipieScores.extend(map(int, str(sum([recipieScores[elf] for elf in elfRecipies]))))
#     return [(elfRecipie + recipieScores[elfRecipie] + 1) % len(recipieScores) for elfRecipie in elfRecipies]

# def compareEnd(recipieScores, pattern):
#     patternLength = len(pattern)
#     if recipieScores[-patternLength:] == pattern:
#         return len(recipieScores) - inpLength
#     if recipieScores[-patternLength-1:-1] == pattern:
#         return len(recipieScores) - patternLength - 1
#     return False

# def findTenAfter(nofRecipies, startingRecipies):
#     recipieScores = startingRecipies[:]
#     nofRecipies = int(nofRecipies)
#     elfRecipies = [0, 1]
#     while len(recipieScores) < 10+nofRecipies:
#         elfRecipies = newRecipies(recipieScores, elfRecipies)
#     return ''.join(map(str, recipieScores[nofRecipies:nofRecipies+10]))

# def numberOfRecipiesBefore(pattern, startingRecipies):
#     pattern = list(map(int, pattern))
#     recipieScores = startingRecipies[:]
#     elfRecipies = [0, 1]
#     while not compareEnd(recipieScores, pattern):
#         elfRecipies = newRecipies(recipieScores, elfRecipies)
#     return compareEnd(recipieScores, pattern)

# print("Solution to day 14 part 1: {}".format(findTenAfter(inp, recipieScores)))
# print("Solution to day 14 part 2: {}".format(numberOfRecipiesBefore(inp, recipieScores)))

from strutils import intToStr

let
    input = "513401"

var
    recipieScores = [3, 7]
    elfRecipies = [0, 1]

proc newRecipies(recipieScore: var seq[int], elfRecipies: var seq[int]) =
    var
        sm = 0
    for elf in elfRecipies:
        sm.inc(recipieScore[elf])
    for c in (sm.intToStr()):
        recipieScore.add(int(c))
    for i in 0..elfRecipies.len:
        let
            elf = elfRecipies[i]
        elfRecipies[i] = (elf + recipieScore[elf] + 1) mod recipieScore.len

# def compareEnd(recipieScores, pattern):
#     patternLength = len(pattern)
#     if recipieScores[-patternLength:] == pattern:
#         return len(recipieScores) - inpLength
#     if recipieScores[-patternLength-1:-1] == pattern:
#         return len(recipieScores) - patternLength - 1
#     return False

proc compareEnd(recipieScores: seq[int], pattern: seq[int]):bool =
    result = (recipieScores[^pattern.len..^1] == pattern) or (recipieScores[^(pattern.len+1)..^1] == pattern)

proc getEnd