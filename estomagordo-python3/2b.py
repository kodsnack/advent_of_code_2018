def solve(d):
	for a in d:
		for b in d:
			diffs = 0
			res = ''

			for i, c in enumerate(a):
				if c != b[i]:
					diffs += 1
					continue
				res += c

			if diffs == 1:
				return res

with open('input_2.txt') as f:
	data = [line.rstrip() for line in f]
	print(solve(data))