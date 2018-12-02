import re
import sys

twos = 0
threes = 0
twos_found = False
threes_found = False
answer = ''


def match(boxid1, boxid2):
    ok = False
    posix = -1

    for ix, (pos1, pos2) in enumerate(zip(boxid1, boxid2)):
        if pos1 != pos2:
            if ok:
                return False
            else:
                ok = True
                posix = ix
    return ok, posix


file = open('input.txt', 'r')
input = file.readlines()
for ix, lines in enumerate(input):
    for ix2, lines2 in enumerate(input):
        if ix != ix2:
            if match(lines, lines2):
                for ixch, ch in enumerate(lines):
                    if ixch != (match(lines, lines2)[1]):
                        answer += ch
                print(answer)
                sys.exit()
