

import numpy as np

from utils import read_input, score_gen, grow


if __name__ == "__main__":

    low_boundary = -10
    gen, trans = read_input(abs(low_boundary))

    scores = grow(gen, trans, low_boundary, 200, verbose="score")

    scores = np.asarray(scores)
    fit = np.polyfit(scores[100:,0], scores[100:,1], 1)
    print(fit, scores[-1,:], fit[1] + scores[-1,0]*fit[0])
    n = 50000000000
    fit_score = fit[1] + n*fit[0]
    print(f"Fitted score after {n} generations are {fit_score}")
