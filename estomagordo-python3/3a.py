from collections import defaultdict


def solve(d):
	seen = defaultdict(int)

	for line in d:
		vals = line.split()
		startx, starty = list(map(int, vals[2][:-1].split(',')))
		width, height = list(map(int, vals[3].split('x')))

		for x in range(width):
			for y in range(height):
				seen[(startx + x, starty + y)] += 1

	return sum(v > 1 for k, v in seen.items())

with open('input_3.txt') as f:
	data = [line.rstrip() for line in f]
	print(solve(data))