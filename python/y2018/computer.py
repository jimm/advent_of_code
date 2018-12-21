from sys import stdout

from utils import pause


@dataclass
class Instruction:
    REG_NAMES = {
        "r[0]": "s",
        "r[1]": "t",
        "r[2]": "w",
        "r[3]": "x",
        "r[4]": "y",
        "r[5]": "z",
    }

    def __init__(self, opcode, a, b, c, opcode_str=None, pc_reg=0):
        self.opcode = opcode
        self.opcode_str = opcode_str or Computer.opcodes[opcode]
        self.pc_reg = pc_reg
        self.a = a
        self.b = b
        self.c = c

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        s = ""
        if self.opcode_str == "addr":
            s = f"r{self.c} = r{self.a} + r{self.b}"
        elif self.opcode_str == "addi":
            s = f"r{self.c} = r{self.a} + {self.b}"
        elif self.opcode_str == "mulr":
            s = f"r{self.c} = r{self.a} * r{self.b}"
        elif self.opcode_str == "muli":
            s = f"r{self.c} = r{self.a} * {self.b}"
        elif self.opcode_str == "banr":
            s = f"r{self.c} = r{self.a} & r{self.b}"
        elif self.opcode_str == "bani":
            s = f"r{self.c} = r{self.a} & {self.b}"
        elif self.opcode_str == "borr":
            s = f"r{self.c} = r{self.a} | r{self.b}"
        elif self.opcode_str == "bori":
            s = f"r{self.c} = r{self.a} | {self.b}"
        elif self.opcode_str == "setr":
            s = f"r{self.c} = r{self.a}"
        elif self.opcode_str == "seti":
            s = f"r{self.c} = {self.a}"
        elif self.opcode_str == "gtir":
            s = f"r{self.c} = {self.a} > r{self.b} ? 1 : 0"
        elif self.opcode_str == "gtri":
            s = f"r{self.c} = r{self.a} > {self.b} ? 1 : 0"
        elif self.opcode_str == "gtrr":
            s = f"r{self.c} = r{self.a} > r{self.b} ? 1 : 0"
        elif self.opcode_str == "eqir":
            s = f"r{self.c} = {self.a} == r{self.b} ? 1 : 0"
        elif self.opcode_str == "eqri":
            s = f"r{self.c} = r{self.a} == {self.b} ? 1 : 0"
        elif self.opcode_str == "eqrr":
            s = f"r{self.c} = r{self.a} == r{self.b} ? 1 : 0"
        else:
            s = "???"
        words = s.split(" ")
        if words[0] == words[2]:
            words = [words[0], f"{words[3]}="] + words[4:]
        if words[0] == words[2] and len(words) == 3:
            if words[1] == "+=":
                words = [words[0], "<<=", "1"]
            elif words[1] == "*=":
                words = [words[0], "^^=", "2"]
        words = [Instruction.REG_NAMES.get(w, w) for w in words]
        return f"{'%-24s' % ' '.join(words)} # {self.opcode_str} {self.a} {self.b} {self.c}"


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
        Instruction.REG_NAMES[f"r[{pc_register}]"] = "pc"
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
        self.regs[i.c] = 1 if (i.a > self.regs[i.b]) else 0

    def gtri(self, i):
        self.regs[i.c] = 1 if (self.regs[i.a] > i.b) else 0

    def gtrr(self, i):
        self.regs[i.c] = 1 if (self.regs[i.a] > self.regs[i.b]) else 0

    def eqir(self, i):
        self.regs[i.c] = 1 if (i.a == self.regs[i.b]) else 0

    def eqri(self, i):
        self.regs[i.c] = 1 if (self.regs[i.a] == i.b) else 0

    def eqrr(self, i):
        self.regs[i.c] = 1 if (self.regs[i.a] == self.regs[i.b]) else 0

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
                getattr(Computer, self.opcode_mapping[inst.opcode]),
                inst.a,
                inst.b,
                inst.c,
                opcode_str=Computer.opcodes[inst.opcode],
                pc_reg=self.pc_register,
            )
        n = 0
        while self.pc >= 0 and self.pc < len(self.program):
            self.dump()  # DEBUG
            pause()  # DEBUG
            # if n % 100_000 == 0:
            #     print(".", end="")
            #     stdout.flush()
            n += 1
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
        print(f"{'%2d' % self.pc}: {'%-32s' % inst} {self.regs}")
