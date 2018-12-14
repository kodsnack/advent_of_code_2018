from typing import Tuple
import re
from day9.CircularDoublyLinkedList import *


def parse(line: str) -> Tuple[int, int]:
    lst = list(map(int, list(filter(None, re.findall(r"\d*", line)))))
    return lst[0], lst[1]


def part1(players: int, last_marble: int) -> int:
    n = Node(0)
    current = (
        0,
        n,
    )
    lst = CircularDoublyLinkedList()
    lst.insert_at_beg(n)

    player_list = {i: 0 for i in range(1, players + 1)}
    for i in range(1, last_marble + 1):
        if i % 23 == 0:
            current = ((current[0]) % players + 1, current[1])
            player_list[current[0]] += i
            prev = current[1]
            for _ in range(7):
                prev = prev.prev

            player_list[current[0]] += prev.data
            current = (current[0], prev.next)
            lst.remove(prev)
            continue

        nd = Node(i)
        lst.insert_after(current[1].next, nd)
        current = ((current[0]) % players + 1, nd)
    return max(player_list.values())


def main():
    with open("input.txt") as f:
        players, last_marble = parse(f.readline())
        print(f"Answer for the first part {part1(players, last_marble)}")
        print(
            f"Answer for the second part {part1(players, last_marble * 100)}")


if __name__ == "__main__":
    main()
