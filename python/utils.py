import os.path
import sys


def read_data_file(day, testing=False, part_num=1):
    if testing:
        fname = f"day{day}_{part_num}_test.txt"
    else:
        fname = f"day{day}.txt"
    path = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), "../data/y2018", fname
    )
    with open(path, "r") as f:
        return f.read()


def data_file_lines(day, testing=False, part_num=1):
    return [line for line in read_data_file(day, testing, part_num).split("\n") if line]
