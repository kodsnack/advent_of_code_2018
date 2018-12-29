"""
Unit tests for December 20, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import unittest

from december20 import Solver


class TestDecember20(unittest.TestCase):
    def test_parser(self):
        inner_str = "^(NEWS|)$"
        s = Solver(from_str=inner_str)
        pathlens = list(s.path_lengths())
        self.assertEqual(max(pathlens), 2)

        inner_str = "^(EE|N)$"
        s = Solver(from_str=inner_str)
        pathlens = list(s.path_lengths())
        self.assertEqual(max(pathlens), 2)

        path_str = "^WNE$"
        s = Solver(from_str=path_str)
        pathlens = list(s.path_lengths())
        self.assertEqual(max(pathlens), 3)

        path_str = "^ENWWW(NEEE|SSE(EE|N))$"
        s = Solver(from_str=path_str)
        pathlens = list(s.path_lengths())
        self.assertEqual(max(pathlens), 10)

        path_str = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
        s = Solver(from_str=path_str)
        pathlens = list(s.path_lengths())
        self.assertEqual(max(pathlens), 23)

        path_str = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
        s = Solver(from_str=path_str)
        pathlens = list(s.path_lengths())
        self.assertEqual(max(pathlens), 31)

    def test_solution(self):
        (one, two) = Solver(from_file="input/december20.input").solve()
        self.assertEqual(one, 3502)
        self.assertEqual(two, 8000)


if __name__ == "__main__":
    unittest.main()
