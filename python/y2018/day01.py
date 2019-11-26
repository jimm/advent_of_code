# Recalibration

import itertools
import sys

from utils import *


def part1(testing=False):
    nums = _read_nums(testing)
    print(sum(nums))


def part2(testing=False):
    nums = _read_nums(testing)
    sum = 0
    seen = {sum}
    for num in itertools.cycle(nums):
        sum += num
        if sum in seen:
            break
        seen.add(sum)
    print(sum)


def _read_nums(testing):
    lines = data_file_lines(2018, 1, testing=testing)
    return [int(line) for line in lines]
