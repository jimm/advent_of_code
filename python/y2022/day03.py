# Rucksack Reorganization

from utils import *


def part1(env):
    lines = data_file_lines(env)
    rucksacks = [rucksack(line) for line in lines]
    commons = [common_item(r) for r in rucksacks]
    print(commons)  # DEBUG
    print(list(priority(ch) for ch in commons))  # DEBUG
    print(sum(priority(ch) for ch in commons))


def part2(env):
    lines = data_file_lines(env)


def rucksack(line):
    half = int(len(line) / 2)
    return (line[:half], line[half:])


def common_item(rucksack):
    a, b = rucksack
    common_set = set(a).intersection(set(b))
    return list(common_set)[0]


def priority(ch):
    print(f"common is {ch}")  # DEBUG
    if ch >= "a" and ch <= "z":
        return ord(ch) - ord("a") + 1
    return ord(ch) - ord("A") + 1
