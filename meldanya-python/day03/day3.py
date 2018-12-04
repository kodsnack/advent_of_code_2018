import sys
import itertools
from collections import namedtuple

SIZE = 1000

Claim = namedtuple("Claim", ["id_", "left", "top", "right", "bottom"])

# Pre-allocate the quantized representation of the fabric
fabric = [[0] * SIZE for _ in range(SIZE)]
claims = []
with open(sys.argv[1]) as f:
    for line in f:
        id_, rest = line.split("@")
        coord, wh = rest.split(":")
        left, top = (int(x) for x in coord.split(","))
        w, h = (int(x) for x in wh.split("x"))
        right = left + w - 1
        bottom = top + h - 1
        claims.append(Claim(id_.strip().replace("#", ""), left, top, right, bottom))
        # Mark what part of the fabric this claim occupies
        x_coords = range(left, right + 1)
        y_coords = range(top, bottom + 1)
        for x, y in itertools.product(x_coords, y_coords):
            fabric[x][y] += 1

# Part 1: Count the squares of the fabric that has two or more claims
contested = [
    1 for x, y in itertools.product(range(SIZE), repeat=2) if fabric[x][y] >= 2
]
print(sum(contested))


# Part 2: Find the one claim that doesn't intersect any other.
def intersects(c1, c2):
    return not (
        c2.left > c1.right
        or c2.right < c1.left
        or c2.top > c1.bottom
        or c2.bottom < c1.top
    )


# Start with all claims and remove the ones that are found to intersect
# another. The last one standing is the right answer.
uniq = set(claims)
for c1, c2 in itertools.combinations(claims, 2):
    if intersects(c1, c2):
        uniq.discard(c1)
        uniq.discard(c2)
assert len(uniq) == 1
print(uniq.pop().id_)
