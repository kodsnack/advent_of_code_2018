
import fileinput


def read_input(low_margin):
    lines = [s for s in fileinput.input()]
    _, initial_str = lines[0].strip().split("initial state: ")
    gen = '.'*low_margin + initial_str + '.'*10*low_margin

    trans = {}
    for line in lines[2:]:
        f, v = line.strip().split(' => ')
        trans[f] = v
    return gen, trans


def grow(gen, trans, low_boundary, ngenerations, verbose):

    scores = list()
    scores.append([0, score_gen(gen, low_boundary)])
    if verbose == "score":
        print(f"0\t{score_gen(gen, low_boundary)}")
    for i in range(ngenerations):
        next = '..'
        for idx in range(2, len(gen)-2):
            spot = gen[idx-2:idx+3]
            next += trans[spot]
        gen = next + "...."
        if verbose == "gen":
            print(next)
        elif verbose == "score":
            print(f"{i+1}\t{score_gen(gen, low_boundary)}")
        scores.append([i + 1, score_gen(gen, low_boundary)])
    return scores


def score_gen(gen, lower_boundary):
    summa = 0
    for i, value in enumerate(gen, lower_boundary):
        if value == '#':
            summa += i
    return summa
