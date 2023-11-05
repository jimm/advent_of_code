# Day 21: Chronal Conversion

from utils import *

from .computer import *


def part1(env):
    c = _read_input(env)
    c.dump_program()


def part2(env):
    c = _read_input(env)


def _read_input(env):
    """Returns tuple containing ([(before, instruction, after)], program)."""
    # data_file_lines strips out all blank lines
    lines = data_file_lines(env)
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
