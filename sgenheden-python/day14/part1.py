

import sys
import blist

from utils import move


if __name__ == "__main__":

    stop = int(sys.argv[1])
    final_len = stop + 10 + 1
    scores = blist.blist([3, 7])
    a = 0
    b = 1

    while len(scores) < final_len:
        new_str = str(int(scores[a] + scores[b]))
        new_scores = list(map(int, list(new_str)))
        scores.extend(new_scores)
        inc_a = scores[a] + 1
        inc_b = scores[b] + 1
        a = move(a, inc_a, scores)
        b = move(b, inc_b, scores)

    s = "".join(map(str,scores[stop:stop+10]))
    print(s)
