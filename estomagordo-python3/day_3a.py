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

def read_and_solve():
	with open('input_3.txt') as f:
		data = [line.rstrip() for line in f]
		return solve(data)

if __name__ == '__main__':
	print(read_and_solve())