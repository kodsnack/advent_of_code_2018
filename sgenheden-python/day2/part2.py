import fileinput


def compare_words(word1, word2):
    idx = -1
    nequal = 0
    for i, (l1, l2) in enumerate(zip(word1, word2)):
        if l1 != l2:
            idx = i
        else:
            nequal += 1
    if nequal == len(word1)-1:
        return word1[:idx]+word1[idx+1:]


if __name__ == "__main__":
    inp = [line.strip() for line in fileinput.input()]
    ret = None
    for i, word1 in enumerate(inp):
        for word2 in inp[i+1:]:
            ret = compare_words(word1, word2)
            if ret is not None:
                print(f"Commong word {ret}")
                break
        if ret is not None:
            break
