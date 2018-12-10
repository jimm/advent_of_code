# Inventory Management System

import collections
import sys

from utils import *


def part1(testing=False):
    lines = data_file_lines(2, 1, testing)
    num_twos = 0
    num_threes = 0
    for line in lines:
        freqs = collections.defaultdict(int)
        for char in line:
            freqs[char] += 1
        vals = list(freqs.values())
        if 2 in vals:
            num_twos += 1
        if 3 in vals:
            num_threes += 1
    print(num_twos * num_threes)


# This is O(n^2) but it's fast enough.
def part2(testing=False):
    lines = data_file_lines(2, 2, testing)
    for line1 in lines:
        for line2 in lines:
            if line1 == line2:
                continue
            num_diffs, same_chars = _compare(line1, line2)
            if num_diffs == 1:
                print("".join(same_chars))
                return


# A generator that returns char or None for each char in line1 that matches
# line2.
def _diff_gen(line1, line2):
    while line1:
        yield (line1[0] == line2[0]) and line1[0] or None
        if len(line1) > 1:
            line1 = line1[1:]
            line2 = line2[1:]
        else:
            line1 = None  # terminate


def _compare(line1, line2):
    num_diffs = 0
    same_chars = []
    for ch in _diff_gen(line1, line2):
        if ch:
            same_chars.append(ch)
        else:
            num_diffs += 1
    return (num_diffs, same_chars)
