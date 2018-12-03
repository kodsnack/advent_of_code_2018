import sys
from collections import namedtuple
import itertools

Claim = namedtuple('Claim', ['id_', 'left', 'top', 'right', 'bottom'])

claims = []
with open(sys.argv[1]) as f:
    for line in f:
        id_, rest = line.split('@')
        coord, wh = rest.split(':')
        left, top = (int(x) for x in coord.split(','))
        w, h = (int(x) for x in wh.split('x'))
        right = left + w - 1
        bottom = top + h - 1
        claims.append(
            Claim(id_.strip().replace('#', ''), left, top, right, bottom)
        )

# Part 1
once = set()
twice = set()
total = 0
for x, y in itertools.product(range(1000), repeat=2):
    for c in claims:
        if not (x > c.right or x < c.left or y > c.bottom or y < c.top):
            if (x, y) in once and (x, y) not in twice:
                twice.add((x, y))
                total += 1
            once.add((x, y))
print(total)


# Part 2
def intersects(c1, c2):
    return not (c2.left > c1.right or
                c2.right < c1.left or
                c2.top > c1.bottom or
                c2.bottom < c1.top)


uniq = set(claims)
for c1, c2 in itertools.combinations(claims, 2):
    if intersects(c1, c2):
        if c1 in uniq:
            uniq.remove(c1)
        if c2 in uniq:
            uniq.remove(c2)
assert len(uniq) == 1
print(uniq.pop().id_)
