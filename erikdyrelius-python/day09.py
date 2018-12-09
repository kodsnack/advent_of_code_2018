from aocbase import readInput
from collections import deque

def marbles(nofMarbels, nofElves):
    board = deque()
    board.append(0)
    theMarble, theElf = 1, 1
    score = dict()
    while theMarble < nofMarbels:
        if theMarble % 23 == 0:
            board.rotate(7)
            score[theElf] = score.get(theElf, 0) + theMarble + board.popleft()
        else:
            board.rotate(-2)
            board.appendleft(theMarble)
        theElf = (theElf % nofElves) + 1
        theMarble += 1
    return max(score.values())

print("Solution to day 9 part 1:", marbles(71863, 493))
print("Solution to day 9 part 2:", marbles(71863*100, 493))