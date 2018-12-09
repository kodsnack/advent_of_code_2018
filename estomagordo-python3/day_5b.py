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
	d = react(d)
	best = len(d)
	for y in range(26):
		char = chr(ord('a') + y)
		polymer = ''.join(c for c in d if c.lower() != char)
		best = min(best, len(react(polymer)))
	return best

def read_and_solve():
	with open('input_5.txt') as f:
		data = [line.rstrip() for line in f]
		return solve(data[0])

if __name__ == '__main__':
	print(read_and_solve())