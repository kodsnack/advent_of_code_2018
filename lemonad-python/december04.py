"""
December 04, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
from datetime import datetime, timedelta
import operator
import re

import numpy as np

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    def parse_lines(self):
        for line in sorted(self.lines()):
            m = re.search("\[(\d+\-\d+\-\d+ \d+:\d+)\] (.+)", line)
            event_date = datetime.fromisoformat(m.group(1))
            event = m.group(2)
            yield event_date, event

    def guard_data(self):
        """Gets data for how long and when guards sleep."""
        guards = {}
        guard_minutes = {}
        current_guard = None
        current_time = None
        asleep_time = None

        for event_date, event in self.parse_lines():
            m = re.match("Guard #(\d+)", event)
            if m:
                current_guard = int(m.group(1))
            elif event == "falls asleep":
                asleep_time = event_date
            elif event == "wakes up":
                wake_time = event_date
                if current_guard not in guards:
                    guards[current_guard] = 0
                    # Only hour after midnight relevant here.
                    guard_minutes[current_guard] = np.zeros(60)
                else:
                    guards[current_guard] += wake_time.minute - asleep_time.minute

                guard_minutes[current_guard][asleep_time.minute : wake_time.minute] += 1
                asleep_time = None

        return guards, guard_minutes

    def solve_part_one(self):
        """Solution for part one."""
        guards, guard_minutes = self.guard_data()

        # Sort by highest sleep time first.
        sleeps_most = sorted(guards.items(), key=operator.itemgetter(1), reverse=True)
        guard_who_sleeps_most = sleeps_most[0][0]
        most_slept_minute = np.argmax(guard_minutes[guard_who_sleeps_most])
        return guard_who_sleeps_most * most_slept_minute

    def solve_part_two(self):
        """Solution for part two."""
        guards, guard_minutes = self.guard_data()

        max_guard = None
        max_minute = None
        max_times = 0
        for guard, minutes in guard_minutes.items():
            # Get the most times a guard was asleep during a specific minute
            # after midnight.
            n = np.max(minutes)
            if n > max_times:
                # Get which minute.
                max_minute = np.argmax(guard_minutes[guard])
                max_times = n
                max_guard = guard
        return max_guard * max_minute

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december04.input")
    (one, two) = s.solve()
    print("ID * minute:", one)
    print("ID * minute:", two)
