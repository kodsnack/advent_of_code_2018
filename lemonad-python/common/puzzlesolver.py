"""
Abstract base class for advent of code puzzle solvers.

"""
from abc import ABC, abstractmethod
import json
import re


class PuzzleSolver(ABC):
    def __init__(self, from_file=None, from_str=None):
        if not (from_file or from_str):
            raise ValueError(
                "PuzzleSolver needs to be initialized from " "either file or string."
            )
        elif from_file and from_str:
            raise ValueError("PuzzleSolver ambiguous initialization.")
        elif from_str:
            self.raw_puzzle_input = from_str.strip()
        else:
            with open(from_file) as f:
                self.raw_puzzle_input = f.read().strip()

    @abstractmethod
    def solve(self):
        pass

    @property
    def puzzle_input(self):
        return self.raw_puzzle_input

    def chars(self):
        for c in self.raw_puzzle_input:
            yield c

    def lines(self, conversion=None):
        for line in self.raw_puzzle_input.split("\n"):
            if not conversion:
                yield line.strip()
            else:
                yield conversion(line.strip())

    def lines_search(self, pattern):
        prog = re.compile(pattern)
        for line in self.raw_puzzle_input.split("\n"):
            m = prog.search(line.strip())
            yield m

    def lines_split(self, split_str, conversion=None):
        for line in self.raw_puzzle_input.split("\n"):
            if not conversion:
                yield line.split(split_str)
            else:
                yield [conversion(x) for x in line.split(split_str)]

    def as_json(self):
        return json.loads(self.raw_puzzle_input)

    def as_int(self):
        return int(self.raw_puzzle_input)

    def as_list(self, split_str=", ", conversion=None):
        for elem in self.raw_puzzle_input.split(split_str):
            if not conversion:
                yield elem
            else:
                yield conversion(elem)

    def as_dict(self):
        d = {}
        for m in self.lines_search("^(.+?)\s*:\s*(.+?)\s*$"):
            if m.group(1) in d:
                raise RuntimeError("Duplicate keys in input")
            d[m.group(1)] = m.group(2)
        return d

    def as_bool_numpy_array(self, false="."):
        """Given a matrix of .'s and #'s, return a boolean numpy array."""
        import numpy as np

        m = []
        for line in self.lines():
            row = (np.fromstring(line, dtype="b") != ord(false)).astype("?")
            m.append(row)
        return np.array(m)

    def as_char_numpy_array(self):
        """Given a matrix of characters return an numpy array."""
        import numpy as np

        m = []
        for line in self.lines():
            row = np.fromstring(line, dtype="c")
            m.append(row)
        return np.array(m)

    def as_instructions(self, separator=","):
        PATTERN = "^\s*(\w+)\s*([^{0}\s]+)*({0}\s*[^{0}\s]+)*".format(separator)
        for m in self.lines_search(PATTERN):
            op = m.group(1)
            args = []
            arg1 = m.group(2)
            if arg1:
                args.append(arg1)

            rest = m.group(3)
            if rest:
                for r in rest.split(","):
                    if not r:
                        continue
                    args.append(r.strip(" ,"))
            yield (op, args)

    def search(self, pattern):
        prog = re.compile(pattern)
        return prog.search(self.raw_puzzle_input)
