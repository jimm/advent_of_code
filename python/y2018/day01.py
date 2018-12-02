# Recalibration

import sys

from utils import *


def part1(testing=False):
    nums = _read_nums(testing)
    print(sum(nums))

def part2_gen(nums):
    while True:
        for num in nums:
            yield num

def part2(testing=False):
    nums = _read_nums(testing)
    sum = 0
    seen = {sum}
    for num in part2_gen(nums):
        sum += num
        if sum in seen:
            break
        seen.add(sum)
    print(sum)

def _read_nums(testing):
    lines = data_file_lines("01", testing)
    return [int(line) for line in lines]
