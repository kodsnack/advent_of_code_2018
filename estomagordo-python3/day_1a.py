def solve(d):
    return sum(map(int, d))


def read_and_solve():
    with open('input_1.txt') as f:
        data = [line.rstrip() for line in f]
        return solve(data)

if __name__ == '__main__':
    print(read_and_solve())