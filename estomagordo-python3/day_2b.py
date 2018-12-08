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

def read_and_solve():
	with open('input_2.txt') as f:
		data = [line.rstrip() for line in f]
		return solve(data)

if __name__ == '__main__':
	print(read_and_solve())