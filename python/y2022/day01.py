# Calorie Counting

import itertools

from utils import *


def part1(env):
    grouped_lines = data_file_lines(env, preserve_blank_lines=True)
    sums = [sum(int(line) for line in nums) for nums in grouped_lines]
    print(sorted(sums)[-1])


def part2(env):
    grouped_lines = data_file_lines(env, preserve_blank_lines=True)
    sums = [sum(int(line) for line in nums) for nums in grouped_lines]
    print(sum(sorted(sums)[-3:]))
