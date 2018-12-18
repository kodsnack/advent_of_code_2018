def part2(inp):
    receipts = [3, 7]
    current_first = 0
    current_second = 1
    current_receipt = ""
    idx = 0
    while True:
        if len(current_receipt) != len(str(inp)):
            current_receipt += str(receipts[idx])
        else:
            current_receipt = current_receipt[1:] + str(receipts[idx])
        if current_receipt == str(inp):
            break
        idx += 1

        elem = receipts[current_first] + receipts[current_second]
        for symbol in str(elem):
            receipts.append(int(symbol))
        current_first = (
            receipts[current_first] + current_first + 1) % len(receipts)
        current_second = (
            receipts[current_second] + current_second + 1) % len(receipts)

    return 1 + idx - len(str(inp))


def part1(inp):
    receipts = [3, 7]
    current_first = 0
    current_second = 1
    while True:
        elem = receipts[current_first] + receipts[current_second]
        for symbol in str(elem):
            receipts.append(int(symbol))
        current_first = (
            receipts[current_first] + current_first + 1) % len(receipts)
        current_second = (
            receipts[current_second] + current_second + 1) % len(receipts)

        if len(receipts) > inp + 10:
            break

    sm = ""
    for i in range(inp, inp + 10):
        sm += str(receipts[i])
    return sm


def solve():
    with open("input.txt") as f:
        inp = int(f.readline())
        print(f"Answer for the first part:\n{part1(inp)}")
        print(f"Answer for the second part:\n{part2(inp)}")


if __name__ == "__main__":
    solve()
