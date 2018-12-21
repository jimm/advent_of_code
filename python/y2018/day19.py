# Puzzle Name

from utils import *

from .computer import Computer, Instruction


def part1(testing=False):
    c = _read_input(1, testing)
    c.execute_program()
    print(c.regs[0])


def part2(testing=False):
    print("run day19.c")


def _read_input(part_num, testing):
    """Returns tuple containing ([(before, instruction, after)], program)."""
    # data_file_lines strips out all blank lines
    lines = data_file_lines(19, part_num, testing)
    line = lines[0]
    pc_register = int(line[4:])
    program = []
    for line in lines[1:]:
        words = line.split(" ")
        words[0] = Computer.opcodes.index(words[0])
        for i in range(1, len(words)):
            words[i] = int(words[i])
        instruction = Instruction(*words)
        program.append(instruction)
    return Computer(program, pc_register=pc_register)
