import string


def clear(word: str) -> str:
    previous = ''
    stack = []
    for char in word:
        if previous == char or previous.lower() != char.lower(
        ) or previous.upper() != char.upper():
            stack.append(char)
            previous = char
            continue

        stack.pop()
        previous = ""
    return "".join(stack)


def part1(line_inp: str) -> int:
    hack = ""
    local_line = line_inp
    while True:
        local_line = clear(local_line)
        if local_line != hack:
            hack = local_line
            continue
        break
    return len(hack)


def part2(line_inp: str) -> int:
    min_list = []
    for char in string.ascii_lowercase:
        min_list.append(
            part1(line_inp.replace(char, "").replace(char.upper(), "")))
    return min(min_list)


if __name__ == "__main__":
    with open("input.txt") as f:
        line = f.readline()
        print(f"Answer for the first part: {part1(line.strip())}")
        print(f"Answer for the second part: {part2(line.strip())}")
