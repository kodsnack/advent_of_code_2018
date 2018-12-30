import fileinput


def count_word(word):
    counts = {}
    for letter in word:
        if letter not in counts:
            counts[letter] = 0
        counts[letter] += 1
    ntwo, nthree = 0, 0
    for _, count in counts.items():
        if count == 2:
            ntwo = 1
        elif count == 3:
            nthree = 1
    return ntwo, nthree


if __name__ == "__main__":
    inp = [line.strip() for line in fileinput.input()]
    ntwo, nthree = 0, 0
    for word in inp:
      a, b = count_word(word)
      ntwo += a
      nthree += b
    checksum = ntwo * nthree
    print(f"Checksum = {checksum} ({ntwo}, {nthree})")
