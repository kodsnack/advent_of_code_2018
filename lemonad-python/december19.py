"""
December 19, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import re

from sympy import ntheory

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    max_reg = 5

    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def addr(A, B, C, regs):
        if A > Solver.max_reg or B > Solver.max_reg or C > Solver.max_reg:
            return None
        regs[C] = regs[A] + regs[B]
        return regs

    @staticmethod
    def addi(A, B, C, regs):
        if A > Solver.max_reg or C > Solver.max_reg:
            return None
        regs[C] = regs[A] + B
        return regs

    @staticmethod
    def mulr(A, B, C, regs):
        if A > Solver.max_reg or B > Solver.max_reg or C > Solver.max_reg:
            return None
        regs[C] = regs[A] * regs[B]
        return regs

    @staticmethod
    def muli(A, B, C, regs):
        if A > Solver.max_reg or C > Solver.max_reg:
            return None
        regs[C] = regs[A] * B
        return regs

    @staticmethod
    def banr(A, B, C, regs):
        if A > Solver.max_reg or B > Solver.max_reg or C > Solver.max_reg:
            return None
        regs[C] = regs[A] & regs[B]
        return regs

    @staticmethod
    def bani(A, B, C, regs):
        if A > Solver.max_reg or C > Solver.max_reg:
            return None
        regs[C] = regs[A] & B
        return regs

    @staticmethod
    def borr(A, B, C, regs):
        if A > Solver.max_reg or B > Solver.max_reg or C > Solver.max_reg:
            return None
        regs[C] = regs[A] | regs[B]
        return regs

    @staticmethod
    def bori(A, B, C, regs):
        if A > Solver.max_reg or C > Solver.max_reg:
            return None
        regs[C] = regs[A] | B
        return regs

    @staticmethod
    def setr(A, B, C, regs):
        if A > Solver.max_reg or C > Solver.max_reg:
            return None
        regs[C] = regs[A]
        return regs

    @staticmethod
    def seti(A, B, C, regs):
        if C > Solver.max_reg:
            return None
        regs[C] = A
        return regs

    @staticmethod
    def gtir(A, B, C, regs):
        if B > Solver.max_reg or C > Solver.max_reg:
            return None
        if A > regs[B]:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def gtri(A, B, C, regs):
        if A > Solver.max_reg or C > Solver.max_reg:
            return None
        if regs[A] > B:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def gtrr(A, B, C, regs):
        if A > Solver.max_reg or C > Solver.max_reg:
            return None
        if regs[A] > regs[B]:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def eqir(A, B, C, regs):
        if B > Solver.max_reg or C > Solver.max_reg:
            return None
        if A == regs[B]:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def eqri(A, B, C, regs):
        if A > Solver.max_reg or C > Solver.max_reg:
            return None
        if regs[A] == B:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def eqrr(A, B, C, regs):
        if A > Solver.max_reg or B > Solver.max_reg or C > Solver.max_reg:
            return None
        if regs[A] == regs[B]:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def exec(program, regs, ipreg, breakpoint):
        instrset = {
            "addr": Solver.addr,
            "addi": Solver.addi,
            "mulr": Solver.mulr,
            "muli": Solver.muli,
            "banr": Solver.banr,
            "bani": Solver.bani,
            "borr": Solver.borr,
            "bori": Solver.bori,
            "setr": Solver.setr,
            "seti": Solver.seti,
            "gtir": Solver.gtir,
            "gtri": Solver.gtri,
            "gtrr": Solver.gtrr,
            "eqir": Solver.eqir,
            "eqri": Solver.eqri,
            "eqrr": Solver.eqrr,
        }

        if ipreg < 0 or ipreg >= len(regs):
            raise Exception("Invalid ipreg")

        while (
            regs[ipreg] >= 0
            and regs[ipreg] < len(program)
            and regs[ipreg] != breakpoint
        ):
            m = re.match("\s*(\w+)\s(\d+)\s(\d+)\s(\d+)", program[regs[ipreg]])
            opcode = m.group(1)
            A = int(m.group(2))
            B = int(m.group(3))
            C = int(m.group(4))
            regs = instrset[opcode](A, B, C, regs.copy())
            regs[ipreg] += 1
        return regs

    def solve_with_breakpoint(self, regs0, breakpoint):
        lines = []
        for line in self.lines():
            lines.append(line)

        m = re.match("\s*#ip\s+(\d)", lines[0])
        if not m:
            raise ("No ip register definition")
        ipreg = int(m.group(1))

        regs = [0] * 6
        regs[0] = regs0
        regs = self.exec(lines[1:], regs.copy(), ipreg, breakpoint)
        if regs[ipreg] != breakpoint:
            return regs[0]
        return sum(ntheory.factor_.divisors(regs[3]))

    def solve_part_one(self):
        """Solution for part one."""
        return self.solve_with_breakpoint(0, 10)

    def solve_part_two(self):
        """Solution for part two."""
        return self.solve_with_breakpoint(1, 34)

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december19.input")
    (one, two) = s.solve()
    print("Value left in register 0 (part one):", one)
    print("Value left in register 0 (part two):", two)
