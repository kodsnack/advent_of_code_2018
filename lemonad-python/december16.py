"""
December 16, Advent of Code 2018 (Jonas Nockert / @lemonad)

"""
import re

from common.puzzlesolver import PuzzleSolver


class Solver(PuzzleSolver):
    MAX_REG = 3

    def __init__(self, *args, **kwargs):
        super(Solver, self).__init__(*args, **kwargs)

    @staticmethod
    def addr(A, B, C, regs):
        if A > Solver.MAX_REG or B > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        regs[C] = regs[A] + regs[B]
        return regs

    @staticmethod
    def addi(A, B, C, regs):
        if A > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        regs[C] = regs[A] + B
        return regs

    @staticmethod
    def mulr(A, B, C, regs):
        if A > Solver.MAX_REG or B > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        regs[C] = regs[A] * regs[B]
        return regs

    @staticmethod
    def muli(A, B, C, regs):
        if A > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        regs[C] = regs[A] * B
        return regs

    @staticmethod
    def banr(A, B, C, regs):
        if A > Solver.MAX_REG or B > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        regs[C] = regs[A] & regs[B]
        return regs

    @staticmethod
    def bani(A, B, C, regs):
        if A > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        regs[C] = regs[A] & B
        return regs

    @staticmethod
    def borr(A, B, C, regs):
        if A > Solver.MAX_REG or B > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        regs[C] = regs[A] | regs[B]
        return regs

    @staticmethod
    def bori(A, B, C, regs):
        if A > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        regs[C] = regs[A] | B
        return regs

    @staticmethod
    def setr(A, B, C, regs):
        if A > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        regs[C] = regs[A]
        return regs

    @staticmethod
    def seti(A, B, C, regs):
        if C > Solver.MAX_REG:
            return None
        regs[C] = A
        return regs

    @staticmethod
    def gtir(A, B, C, regs):
        if B > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        if A > regs[B]:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def gtri(A, B, C, regs):
        if A > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        if regs[A] > B:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def gtrr(A, B, C, regs):
        if A > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        if regs[A] > regs[B]:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def eqir(A, B, C, regs):
        if B > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        if A == regs[B]:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def eqri(A, B, C, regs):
        if A > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        if regs[A] == B:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    @staticmethod
    def eqrr(A, B, C, regs):
        if A > Solver.MAX_REG or B > Solver.MAX_REG or C > Solver.MAX_REG:
            return None
        if regs[A] == regs[B]:
            regs[C] = 1
        else:
            regs[C] = 0
        return regs

    def solve_part_one(self):
        """Solution for part one."""
        instrset = set(
            [
                self.addr,
                self.addi,
                self.mulr,
                self.muli,
                self.banr,
                self.bani,
                self.borr,
                self.bori,
                self.setr,
                self.seti,
                self.gtir,
                self.gtri,
                self.gtrr,
                self.eqir,
                self.eqri,
                self.eqrr,
            ]
        )
        lines = []
        for line in self.lines():
            lines.append(line)

        lineno = 0
        mops_counter = 0
        mops = 0
        while lineno < len(lines):
            if not lines[lineno]:
                break
            m = re.search("Before:\s+\[(.+?)\]", lines[lineno])
            regs_before = [int(n) for n in m.group(1).split(",")]
            instruction = [int(n) for n in lines[lineno + 1].split(" ")]
            opcode = instruction[0]
            A = instruction[1]
            B = instruction[2]
            C = instruction[3]
            m = re.search("After:\s+\[(.+?)\]", lines[lineno + 2])
            regs_after = [int(n) for n in m.group(1).split(",")]

            matching_opcodes = 0
            for instr in instrset:
                regs = instr(A, B, C, regs_before.copy())
                if regs == regs_after:
                    matching_opcodes += 1

            if matching_opcodes >= 3:
                mops_counter += 1

            lineno += 4
            mops += 1
        return mops_counter

    def solve_part_two(self):
        """Solution for part two."""
        instrset = set(
            [
                self.addr,
                self.addi,
                self.mulr,
                self.muli,
                self.banr,
                self.bani,
                self.borr,
                self.bori,
                self.setr,
                self.seti,
                self.gtir,
                self.gtri,
                self.gtrr,
                self.eqir,
                self.eqri,
                self.eqrr,
            ]
        )

        valid_instructions = {}
        for i in range(16):
            for instr in instrset:
                valid_instructions[i] = instrset.copy()

        lines = []
        for line in self.lines():
            lines.append(line)

        lineno = 0
        mops_counter = 0
        mops = 0
        while lineno < len(lines):
            if not lines[lineno]:
                break
            m = re.search("Before:\s+\[(.+?)\]", lines[lineno])
            regs_before = [int(n) for n in m.group(1).split(",")]
            instruction = [int(n) for n in lines[lineno + 1].split(" ")]
            opcode = instruction[0]
            A = instruction[1]
            B = instruction[2]
            C = instruction[3]
            m = re.search("After:\s+\[(.+?)\]", lines[lineno + 2])
            regs_after = [int(n) for n in m.group(1).split(",")]

            invalid_instr = set()
            for instr in valid_instructions[opcode]:
                regs = instr(A, B, C, regs_before.copy())
                if regs != regs_after:
                    invalid_instr.add(instr)
            valid_instructions[opcode].difference_update(invalid_instr)

            lineno += 4
            mops += 1

        instrset = [None] * 16
        while True:
            found = False
            for i in range(16):
                if len(valid_instructions[i]) == 1:
                    found = True
                    instrset[i] = valid_instructions[i].pop()
                    for j in range(16):
                        valid_instructions[j].difference_update([instrset[i]])
            if not found:
                break

        lineno += 2
        begin = lineno
        regs = [0, 0, 0, 0]
        while lineno < len(lines):
            if not lines[lineno].strip():
                break
            instruction = [int(n) for n in lines[lineno].split(" ")]
            opcode = instruction[0]
            A = instruction[1]
            B = instruction[2]
            C = instruction[3]
            regs = instrset[opcode](A, B, C, regs)
            if regs is None:
                break
            lineno += 1
        return regs[0]

    def solve(self):
        return (self.solve_part_one(), self.solve_part_two())


if __name__ == "__main__":
    s = Solver(from_file="input/december16.input")
    (one, two) = s.solve()
    print("Number of samples behaving like three or more opcodes:", one)
    print("Register 0 after executing the test program:", two)
