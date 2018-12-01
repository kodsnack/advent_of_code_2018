def solve(d):
	count = 0
	reached = set([0])

	while True:
		for line in d:
			count += int(line)
			if count in reached:
				return count
			reached.add(count)

with open('input_1.txt') as f:
	data = [line.rstrip() for line in f]
	print(solve(data))