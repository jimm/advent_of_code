# Recalibration

import itertools
import sys

from utils import *


def part1(env):
    nums = _read_nums(env)
    print(sum(nums))


def part2(env):
    nums = _read_nums(env)
    sum = 0
    seen = {sum}
    for num in itertools.cycle(nums):
        sum += num
        if sum in seen:
            break
        seen.add(sum)
    print(sum)


def _read_nums(env):
    lines = data_file_lines(env)
    return [int(line) for line in lines]
