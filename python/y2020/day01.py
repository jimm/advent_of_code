# Puzzle Name

import os.path

from utils import *

YEAR = file_year(__file__)
DAY = file_day(__file__)


def part1(testing=False):
    entries = [int(line) for line in data_file_lines(YEAR, DAY, 1, testing)]
    i = 0
    for e1 in entries:
        for e2 in entries[i + 1 :]:
            if e1 + e2 == 2020:
                print(e1 * e2)
                exit(0)
        i += 1


def part2(testing=False):
    entries = [int(line) for line in data_file_lines(YEAR, DAY, 1, testing)]
    i = 0
    for e1 in entries:
        for e2 in entries[i + 1 :]:
            for e3 in entries[i + 2 :]:
                if e1 + e2 + e3 == 2020:
                    print(e1 * e2 * e3)
                    exit(0)
        i += 1
