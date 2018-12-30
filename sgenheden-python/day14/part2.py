

import sys
import blist


from utils import move


if __name__ == "__main__":

    stop = sys.argv[1]
    scores = "37"
    scores_int = blist.blist([3, 7])
    a = 0
    b = 1

    while True:
        ia = scores_int[a]
        ib = scores_int[b]
        new_int = ia + ib
        new_str = str(new_int)
        scores += new_str
        scores_int.extend(list(map(int, new_str)))
        inc_a = ia + 1
        inc_b = ib + 1
        a = move(a, inc_a, scores)
        b = move(b, inc_b, scores)

        if len(scores) >= len(stop):
            if scores[-len(stop):] == stop or scores[-len(stop) - 1:-1] == stop:
                idx = scores.find(stop)
                print(f"Stop found at {idx}")
                break
