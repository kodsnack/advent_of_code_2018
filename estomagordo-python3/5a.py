def react(d):
	while True:
		removals = []
		l = len(d)
		x = 0
		while x < l -1:
			if d[x] != d[x + 1] and d[x].upper() == d[x + 1].upper():
				removals.append(x)
				x += 1
			x += 1
		if not removals:
			return d
		
		e = str(d)
		d = e[:removals[0]]

		for i in range(1, len(removals)):
			d += e[removals[i - 1] + 2:removals[i]]

		d += e[removals[-1] + 2:]


def solve(d):
	return len(react(d))

with open('input_5.txt') as f:
	data = [line.rstrip() for line in f]
	print(solve(data[0]))