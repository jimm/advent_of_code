# Chronal Classification

import collections

from utils import *

Instruction = collections.namedtuple("Instruction", ["opcode", "a", "b", "c"])


class Computer:
    opcodes = [
        "addr",
        "addi",
        "mulr",
        "muli",
        "banr",
        "bani",
        "borr",
        "bori",
        "setr",
        "seti",
        "gtir",
        "gtri",
        "gtrr",
        "eqir",
        "eqri",
        "eqrr",
    ]

    def __init__(self, before_and_after_tuples, program=[]):
        self.regs = [0, 0, 0, 0]
        self.before_and_after_tuples = before_and_after_tuples
        self.program = program
        self.opcode_mapping = [0] * 16

    def addr(self, i):
        self.regs[i.c] = self.regs[i.a] + self.regs[i.b]

    def addi(self, i):
        self.regs[i.c] = self.regs[i.a] + i.b

    def mulr(self, i):
        self.regs[i.c] = self.regs[i.a] * self.regs[i.b]

    def muli(self, i):
        self.regs[i.c] = self.regs[i.a] * i.b

    def banr(self, i):
        self.regs[i.c] = self.regs[i.a] & self.regs[i.b]

    def bani(self, i):
        self.regs[i.c] = self.regs[i.a] & i.b

    def borr(self, i):
        self.regs[i.c] = self.regs[i.a] | self.regs[i.b]

    def bori(self, i):
        self.regs[i.c] = self.regs[i.a] | i.b

    def setr(self, i):
        self.regs[i.c] = self.regs[i.a]

    def seti(self, i):
        self.regs[i.c] = i.a

    def gtir(self, i):
        self.regs[i.c] = i.a > self.regs[i.b] and 1 or 0

    def gtri(self, i):
        self.regs[i.c] = self.regs[i.a] > i.b and 1 or 0

    def gtrr(self, i):
        self.regs[i.c] = self.regs[i.a] > self.regs[i.b] and 1 or 0

    def eqir(self, i):
        self.regs[i.c] = i.a == self.regs[i.b] and 1 or 0

    def eqri(self, i):
        self.regs[i.c] = self.regs[i.a] == i.b and 1 or 0

    def eqrr(self, i):
        self.regs[i.c] = self.regs[i.a] == self.regs[i.b] and 1 or 0

    def is_versatile(self, bia):
        """Returns true if instruction could be one of 3 or more opcodes."""
        return len(self.possible_opcodes(bia)) >= 3

    def determine_opcodes(self):
        opcode_matches = [set() for i in range(16)]
        for bia in self.before_and_after_tuples:
            before, instruction, after = bia
            op = instruction.opcode
            if len(opcode_matches[op]) == 1:
                continue
            possible = set(self.possible_opcodes(bia)[:])
            if len(opcode_matches[op]) == 0:
                opcode_matches[op].update(possible.copy())
            else:
                opcode_matches[op] = opcode_matches[op] & possible.copy()
        self.opcode_mapping = self.reduce_opcode_matches(opcode_matches)

    def call_named(self, name, instruction):
        f = getattr(Computer, name)
        f(self, instruction)

    def execute_program(self):
        self.regs = [0, 0, 0, 0]
        for i in self.program:
            self.call_named(self.opcode_mapping[i.opcode], i)

    def possible_opcodes(self, bia):
        before, instruction, after = bia
        poss_ops = []
        old_regs = self.regs[:]
        for name in Computer.opcodes:
            self.regs = before[:]
            self.call_named(name, instruction)
            if self.regs == after:
                poss_ops.append(name)
        self.regs = old_regs[:]
        return poss_ops

    def reduce_opcode_matches(self, matches):
        reduced = [None] * 16
        while None in reduced:
            for i, match_set in enumerate(matches):
                if len(match_set) == 1:
                    name = list(match_set)[0]
                    reduced[i] = name
                    for j in range(16):
                        matches[j].discard(name)
        return reduced

def part1(testing=False):
    before_and_after_tuples, _ = _read_input(testing)
    c = Computer(before_and_after_tuples, [])
    if testing:
        print(_versatile(before_and_after_tuples[0], True))
    else:
        print(len([1 for bia in before_and_after_tuples if c.is_versatile(bia)]))


def part2(testing=False):
    before_and_after_tuples, program = _read_input(testing)
    c = Computer(before_and_after_tuples, program)
    c.determine_opcodes()
    c.execute_program()
    print(c.regs[0])


def _read_input(testing):
    """Returns tuple containing ([(before, instruction, after)], program)."""
    # data_file_lines strips out all blank lines
    lines = data_file_lines(16, 1, testing)
    bia_tuples = []
    while lines and lines[0].startswith("Before: ["):
        before = [
            int(lines[0][9:10]),
            int(lines[0][12:13]),
            int(lines[0][15:16]),
            int(lines[0][18:19]),
        ]
        vals = [int(word) for word in lines[1].split(" ")]
        instruction = Instruction(*vals)
        after = [
            int(lines[2][9:10]),
            int(lines[2][12:13]),
            int(lines[2][15:16]),
            int(lines[2][18:19]),
        ]
        bia_tuples.append((before, instruction, after))
        lines = lines[3:]
    program = []
    for line in lines:
        vals = [int(word) for word in line.split(" ")]
        instruction = Instruction(*vals)
        program.append(instruction)
    return (bia_tuples, program)
