from collections import namedtuple

Instruction = namedtuple("Instruction", ["opcode", "a", "b", "c"])


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

    def __init__(self, program, pc_register=5, before_and_after_tuples=None):
        self.regs = [0, 0, 0, 0, 0, 0]
        self.pc = 0
        self.pc_register = pc_register
        self.before_and_after_tuples = before_and_after_tuples
        self.program = program
        self.opcode_mapping = Computer.opcodes[:]

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
        self.regs[self.pc_register] = self.pc
        f = getattr(Computer, name)
        f(self, instruction)
        self.pc = self.regs[self.pc_register] + 1

    def execute_program(self, starting_regs=[0, 0, 0, 0, 0, 0]):
        self.regs = starting_regs
        self.pc = 0
        for pc, inst in enumerate(self.program):
            self.program[pc] = Instruction(
                getattr(Computer, self.opcode_mapping[inst.opcode]), *inst[1:]
            )
        while self.pc >= 0 and self.pc < len(self.program):
            self.regs[self.pc_register] = self.pc
            instruction = self.program[self.pc]
            instruction.opcode(self, instruction)
            self.pc = self.regs[self.pc_register] + 1

    def possible_opcodes(self, bia):
        before, instruction, after = bia
        extra = [0] * (len(self.regs) - len(before))
        poss_ops = []
        old_regs = self.regs[:]
        for name in Computer.opcodes:
            self.regs = before[:] + extra
            self.call_named(name, instruction)
            if self.regs[: len(before)] == after:
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

    def dump(self):
        inst = (
            self.program[self.pc]
            if (self.pc >= 0 and self.pc < len(self.program))
            else "???"
        )
        print(f"pc {self.pc}, pc_reg {self.pc_register}, regs {self.regs}, inst {inst}")
