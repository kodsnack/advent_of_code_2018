

def make_index(current, step, circle):
    trial = current + step
    if trial >= len(circle):
        return trial - len(circle)
    elif trial < 0:
        return len(circle) + trial
    else:
        return trial


def play(nplayers, nmarbles, cirlce_class):

    circle = cirlce_class([0, 1])
    score = {i+1: 0 for i in range(nplayers)}
    current_player = 2
    current_marble = 1
    for marble_value in range(2, nmarbles+1):
        if marble_value % 23 == 0:
            idx = make_index(current_marble, -7, circle)
            score[current_player] += marble_value + circle.pop(idx)
            current_marble = idx
        else:
            idx = make_index(current_marble, 1, circle)
            circle.insert(idx+1, marble_value)
            current_marble = idx+1
        current_player += 1
        if current_player > nplayers:
            current_player = 1
        #circle_str = ''.join(f" {x} " if i != current_marble else  f" ({x}) " for i, x in enumerate(circle))
        #print(f"{current_player}, {current_marble}, {circle_str}")

    return score
