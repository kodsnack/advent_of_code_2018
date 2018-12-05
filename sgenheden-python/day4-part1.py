

import numpy as np

from day4_utils import read_stdin, parse_events


if __name__ == "__main__":
    data = read_stdin()
    guard_events = parse_events(data)

    guard_sum = []
    for key, events in guard_events.items():
        mat = np.array(events)
        sum_mat = mat.sum(axis=0)
        print(f"Guard {key} is asleep {sum_mat.sum()} at most at minute {sum_mat.argmax()}")
        guard_sum.append((key, sum_mat.sum(), sum_mat.argmax()))

    guard_sum.sort(key=lambda x: x[1], reverse=True)
    print("\n\n")
    print(f"Guard {guard_sum[0][0]} is asleep {guard_sum[0][1]} at most at minute {guard_sum[0][2]}")
    print(f"Answer is {int(guard_sum[0][0])*guard_sum[0][2]}")
