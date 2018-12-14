import sys
from collections import Counter

with open(sys.argv[1]) as f:
    ids = [l.strip() for l in f.readlines()]


# Part 1
counters = [Counter(id_) for id_ in ids]
twice = set()
thrice = set()
for id_, c in zip(ids, counters):
    if 2 in c.values():
        twice.add(id_)
    if 3 in c.values():
        thrice.add(id_)

print(len(twice) * len(thrice))


# Part 2
def diff_same(l1, l2):
    diff, same = [], []
    for i, ch in enumerate(l1):
        same.append(ch) if ch == l2[i] else diff.append(ch)
    return ''.join(diff), ''.join(same)


ids = set(ids)
while ids:
    id1 = ids.pop()
    for id2 in ids:
        diff, same = diff_same(id1, id2)
        if len(diff) == 1:
            print(same)
