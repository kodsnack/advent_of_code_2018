
import sys

def move(curr, inc, lst):

    next_ = curr + inc
    while next_ >= len(lst):
        next_ -= len(lst)
    return next_


def print_scores(scores, a, b):
    for i, score in enumerate(scores):
        if i == a:
            sys.stdout.write(f" ({score}) ")
        elif i == b:
            sys.stdout.write(f" [{score}] ")
        else:
            sys.stdout.write(f" {score} ")
    print("")
