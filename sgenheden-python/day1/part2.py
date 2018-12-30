
from utils import stdin2ndarray


def find_repeating(inp):
    freq = 0
    freq_list = []
    while True:
        for change in inp:
            freq += change
            if freq in freq_list:
                print(f"First found {freq} repeated")
                return
            freq_list.append(freq)


if __name__ == "__main__":
    inp = stdin2ndarray()
    find_repeating(inp)
