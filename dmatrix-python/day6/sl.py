import numpy as np
from typing import List


def manhattan_distance(p, q):
    return sum([abs(p[i] - q[i]) for i in range(len(p))])


def part2(lines: List[List[int]]) -> int:
    xmax = max(coord[0] for coord in lines)
    xmin = min(coord[0] for coord in lines)
    ymax = max(coord[1] for coord in lines)
    ymin = min(coord[1] for coord in lines)
    nin = 0
    for x in range(xmin, xmax + 1):
        for y in range(ymin, ymax + 1):
            distance_list = np.array(
                [manhattan_distance([x, y], coord) for coord in lines])
            if distance_list.sum() < 10000:
                nin += 1
    return nin


def part1(lines: List[List[int]]) -> int:
    xmax = max(coord[0] for coord in lines)
    xmin = min(coord[0] for coord in lines)
    ymax = max(coord[1] for coord in lines)
    ymin = min(coord[1] for coord in lines)
    cnt = [0] * len(lines)
    for x in range(xmin, xmax + 1):
        for y in range(ymin, ymax + 1):
            distance_list = np.array(
                [manhattan_distance([x, y], coord) for coord in lines])
            min_count = np.sum(distance_list == distance_list.min())
            if min_count > 1:
                continue
            cnt[int(np.argmin(distance_list))] += 1 if not (
                x == xmax or x == xmin or y == ymax or y == ymin) else 1e6
    cnt_array = np.array(cnt, dtype=np.int32)
    return cnt_array[cnt_array < 1e6].max()


if __name__ == "__main__":
    with open("input.txt") as f:
        my_lines = list(map(lambda r: list(map(int, r.strip().split(','))), f.readlines()))
        test_lines = [[1, 1], [1, 6], [8, 3], [3, 4], [5, 5], [8, 9]]
        print(f"Answer for the first part: {part1(my_lines)}")
        print(f"Answer for the second part: {part2(my_lines)}")
