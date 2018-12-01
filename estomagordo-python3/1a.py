def solve(d):
	return sum(map(int, d))

with open('input_1.txt') as f:
	data = [line.rstrip() for line in f]
	print(solve(data))