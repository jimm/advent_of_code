# Calorie Counting

import itertools

from utils import *


def part1(testing=False):
    grouped_lines = data_file_lines(2022, 1, part_num=1, preserve_blank_lines=True, testing=testing)
    sums = [sum(int(line) for line in nums) for nums in grouped_lines]
    print(sorted(sums)[-1])

def part2(testing=False):
    grouped_lines = data_file_lines(2022, 1, part_num=1, preserve_blank_lines=True, testing=testing)
    sums = [sum(int(line) for line in nums) for nums in grouped_lines]
    print(sum(sorted(sums)[-3:]))
