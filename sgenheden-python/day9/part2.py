
import fileinput
import re

from blist import blist

from utils import play


if __name__ == "__main__":

    m = re.match(f"(.+) players; last marble is worth (.+) points", list(fileinput.input())[0])
    nplayers, nmarbles = int(m.group(1)), int(m.group(2))
    score = play(nplayers, nmarbles*100, blist)

    players = list(score.keys())
    players.sort(key=lambda x: score[x], reverse=True)
    print(f"Player {players[0]} has highest score with {score[players[0]]}")
