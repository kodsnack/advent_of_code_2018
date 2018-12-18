
from utils import read_input, operations


if __name__ == "__main__":

    befores, instructions, afters, _ = read_input()
    nsolution = 0
    for be, ins, aft in zip(befores, instructions, afters):
        nmatches = 0
        for op, func in operations.items():
            try:
                res = func(be, ins)
                if res == aft:
                    nmatches += 1
            except IndexError:
                pass
        if nmatches >= 3:
            nsolution += 1
    print(f"{nsolution} samples matches 3 or more operations of {len(befores)} samples")
