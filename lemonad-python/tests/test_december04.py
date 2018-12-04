"""
Unit tests for December 04, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december04 import Solver


class TestDecember04(unittest.TestCase):
    def test_example(self):
        input_data = """
        [1518-11-01 00:00] Guard #10 begins shift
        [1518-11-01 00:05] falls asleep
        [1518-11-01 00:25] wakes up
        [1518-11-01 00:30] falls asleep
        [1518-11-01 00:55] wakes up
        [1518-11-01 23:58] Guard #99 begins shift
        [1518-11-02 00:40] falls asleep
        [1518-11-02 00:50] wakes up
        [1518-11-03 00:05] Guard #10 begins shift
        [1518-11-03 00:24] falls asleep
        [1518-11-03 00:29] wakes up
        [1518-11-04 00:02] Guard #99 begins shift
        [1518-11-04 00:36] falls asleep
        [1518-11-04 00:46] wakes up
        [1518-11-05 00:03] Guard #99 begins shift
        [1518-11-05 00:45] falls asleep
        [1518-11-05 00:55] wakes up
        """
        s = Solver(from_str=input_data)
        (one, two) = s.solve()
        self.assertEqual(one, 240)
        self.assertEqual(two, 4455)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december04.input").solve()
        self.assertEqual(one, 84636)
        self.assertEqual(two, 91679)


if __name__ == "__main__":
    unittest.main()
