# Chronal Classification

from utils import *

from .computer import Computer, Instruction


def part1(testing=False):
    before_and_after_tuples, _ = _read_input(testing)
    c = Computer([], before_and_after_tuples=before_and_after_tuples)
    if testing:
        print(c.is_versatile(before_and_after_tuples[0]))
    else:
        print(len([1 for bia in before_and_after_tuples if c.is_versatile(bia)]))


def part2(testing=False):
    before_and_after_tuples, program = _read_input(testing)
    c = Computer(program, before_and_after_tuples=before_and_after_tuples)
    c.determine_opcodes()
    c.execute_program()
    print(c.regs[0])


def _read_input(testing):
    """Returns tuple containing ([(before, instruction, after)], program)."""
    # data_file_lines strips out all blank lines
    lines = data_file_lines(2018, 16, 1, testing)
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
