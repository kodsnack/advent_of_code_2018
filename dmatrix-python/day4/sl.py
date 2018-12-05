from typing import List
from collections import defaultdict
import re


# Part 1
def part1(lines: List[str]) -> int:
    d = defaultdict(lambda: defaultdict(int))
    current = None
    start_time = 0
    for line in lines:
        match = re.search("#\d*", line)
        if match:
            current = match.group()
            continue
        match = re.search("falls", line)
        if match:
            start_time = int(re.search(":\d{2}", line).group().lstrip(':'))
            continue
        end_time = int(re.search(":\d{2}", line).group().lstrip(':'))
        for i in range(start_time, end_time):
            d[current][i] += 1
    sleepy_guard = sorted(
        d.items(), key=lambda kv: sum(kv[1].values()), reverse=True)[0]
    return int(sleepy_guard[0].lstrip("#")) * sorted(
        sleepy_guard[1].items(), key=lambda kv: kv[1], reverse=True)[0][0]


# Part 1
def part2(lines: List[str]) -> int:
    d = defaultdict(lambda: defaultdict(int))
    current = None
    start_time = 0
    for line in lines:
        match = re.search("#\d*", line)
        if match:
            current = match.group()
            continue
        match = re.search("falls", line)
        if match:
            start_time = int(re.search(":\d{2}", line).group().lstrip(':'))
            continue
        end_time = int(re.search(":\d{2}", line).group().lstrip(':'))
        for i in range(start_time, end_time):
            d[current][i] += 1
    sleepy_minute = max(d.items(), key=lambda kv: max(kv[1].values()))
    return int(sleepy_minute[0].lstrip("#")) * max(
        sleepy_minute[1].items(), key=lambda tpl: tpl[1])[0]


if __name__ == "__main__":
    with open("input.txt") as f:
        my_lines = list(map(lambda r: r.strip(), f.readlines()))
        my_lines.sort(key=lambda l: l[1:17])
        print(f"Answer for the first part: {part1(my_lines)}")
        print(f"Answer for the second part: {part2(my_lines)}")
