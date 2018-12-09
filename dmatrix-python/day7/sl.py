from typing import List, Tuple
from collections import defaultdict


def parse(line: str) -> Tuple[str, str]:
    return line[36], line[5]


def part2(lines: List[str], worker_num, seconds) -> int:
    list_of_steps = defaultdict(list)
    for line in lines:
        parsed_line = parse(line)
        list_of_steps[parsed_line[0]].append(parsed_line[1])
    flat_list = list(
        set([item for sublist in list_of_steps.values() for item in sublist]))
    keys = list(list_of_steps.keys())
    final = list((set(flat_list).union(set(keys))) - set(keys))
    for fin in final:
        list_of_steps[fin] = []
    list_of_steps = dict(
        sorted({k: v
                for k, v in list_of_steps.items()}.items()))
    str_list = []
    workers = [['', -1] for _ in range(worker_num)]
    count = 0
    while True:
        available_tasks = [
            k for k, v in list_of_steps.items()
            if len(v) == 0 and k not in str_list
        ]
        for worker in workers:
            if worker[1] <= 0 and len(available_tasks) >= 1:
                worker[0] = available_tasks.pop(0)
                worker[1] = ord(worker[0]) - 64 + seconds
                str_list.append(worker[0])

        if len([w[1] for w in workers if w[1] != -1]) == 0:
            break

        min_amount = min([w[1] for w in workers if w[1] != -1])
        count += min_amount
        for worker in workers:
            if worker[1] >= min_amount:
                worker[1] -= min_amount
                if worker[1] <= 0:
                    worker[1] = -1
                    list_of_steps = {
                        k: list(filter(lambda x: x != worker[0], v))
                        for k, v in list_of_steps.items()
                    }
    return count


def part1(lines: List[str]) -> str:
    list_of_steps = defaultdict(list)
    for line in lines:
        parsed_line = parse(line)
        list_of_steps[parsed_line[0]].append(parsed_line[1])
    flat_list = list(
        set([item for sublist in list_of_steps.values() for item in sublist]))
    keys = list(list_of_steps.keys())
    final = list((set(flat_list).union(set(keys))) - set(keys))
    for fin in final:
        list_of_steps[fin] = []
    list_of_steps = dict(
        sorted({k: v
                for k, v in list_of_steps.items()}.items()))
    str_list = []

    while sum(list(map(len, list_of_steps.values()))) > 0:
        cur_key = [
            k for k, v in list_of_steps.items()
            if len(v) == 0 and k not in str_list
        ][0]
        str_list.append(cur_key)
        list_of_steps = {
            k: list(filter(lambda x: x != cur_key, v))
            for k, v in list_of_steps.items()
        }
    not_included = [k for k in list_of_steps.keys() if k not in str_list]
    return "".join(str_list + not_included)


if __name__ == "__main__":
    with open("input.txt") as f:
        my_lines = list(map(lambda r: r.strip(), f.readlines()))
        print(f"Answer for the first part {part1(my_lines)}")
        print(f"Answer for the first part {part2(my_lines, 5, 60)}")
