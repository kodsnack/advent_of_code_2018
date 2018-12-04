import sys

with open(sys.argv[1]) as f:
    freqs = [int(l) for l in f]

freq = sum(freqs, 0)
print(freq)

freq = 0
visited = set()
run = True
while run:
    for f in freqs:
        freq += f
        if freq in visited:
            print(freq)
            run = False
            break
        visited.add(freq)
