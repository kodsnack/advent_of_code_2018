from collections import Counter

def solve(d):
	extwo = 0
	exthree = 0

	for line in d:
		c = Counter(line)
		if any(c[key] == 2 for key in c):
			extwo += 1
		if any(c[key] == 3 for key in c):
			exthree += 1

	return extwo * exthree

with open('input_2.txt') as f:
	data = [line.rstrip() for line in f]
	print(solve(data))