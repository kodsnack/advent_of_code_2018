import sys

with open(sys.argv[1]) as f:
    polymer = list(f.read().strip())


def can_react(u1, u2):
    return u1 != u2 and u1.lower() == u2.lower()


def react(polymer):
    i = 0
    while i < len(polymer) - 1:
        if can_react(polymer[i], polymer[i+1]):
            polymer.pop(i)
            # Pop same index since all list element moved back one item
            # with previous pop.
            polymer.pop(i)
            i -= 1
        else:
            i += 1
    return polymer


#
# Part 1
print(len(react(polymer)))


#
# Part 2
def remove_unit(unit, polymer):
    return [p for p in polymer if p.lower() != u]


units = set(p.lower() for p in polymer)
scores = {}
for u in units:
    scores[u] = len(react(remove_unit(u, polymer)))
mini = min(scores.items(), key=lambda x: x[1])
print(mini)
