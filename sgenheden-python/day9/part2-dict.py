
import fileinput
import re


def circle_str(circle, cursor, current):

    cursor_str = f" {cursor} " if cursor != current else f" ({cursor}) "
    if circle[cursor]["next"] == 0:
        return cursor_str
    else:
        return cursor_str + circle_str(circle, circle[cursor]["next"], current)


def step_back(circle, current_marble):
    curr = circle[current_marble]
    value = None
    for i in range(7):
        value = curr["prev"]
        curr = circle[value]
    return value


def play(nplayers, nmarbles):

    circle = {
       0: {
           "prev": 1,
           "next": 1,
       },
        1: {
           "prev": 0,
           "next": 0,
       },
    }
    score = {i+1: 0 for i in range(nplayers)}
    current_player = 2
    current_marble = 1
    for marble_value in range(2, nmarbles+1):
        if marble_value % 23 == 0:
            value = step_back(circle, current_marble)
            marble = circle[value]
            score[current_player] += marble_value + value
            circle[marble["prev"]]["next"] = marble["next"]
            circle[marble["next"]]["prev"] = marble["prev"]
            current_marble = marble["next"]
            del circle[value]
        else:
            marble = circle[current_marble]
            next_marble = circle[marble["next"]]
            next_next_marble = circle[next_marble["next"]]
            new = {
                "prev": marble["next"],
                "next": next_marble["next"]
            }
            next_marble["next"] = marble_value
            next_next_marble["prev"] = marble_value
            circle[marble_value] = new
            current_marble = marble_value
        current_player += 1
        if current_player > nplayers:
            current_player = 1
        #print(f"{current_player}, {current_marble}, {circle_str(circle, 0, current_marble)}")


    return score


if __name__ == "__main__":

    m = re.match(f"(.+) players; last marble is worth (.+) points", list(fileinput.input())[0])
    nplayers, nmarbles = int(m.group(1)), int(m.group(2))
    score = play(nplayers, nmarbles*100)

    players = list(score.keys())
    players.sort(key=lambda x: score[x], reverse=True)
    print(f"Player {players[0]} has highest score with {score[players[0]]}")
