import os.path
import sys


def read_data_file(day, part_num=1, testing=False):
    fname = f"day{'%02d' % day}"
    if testing:
        fname += f"_{part_num}_test"
    fname += ".txt"
    path = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), "../data/y2018", fname
    )
    with open(path, "r") as f:
        return f.read()


def data_file_lines(day, part_num=1, testing=False):
    return [line for line in read_data_file(day, part_num, testing).split("\n") if line]


def minmax(xs):
    min_val = None
    max_val = None
    for x in xs:
        if min_val is None or x < min_val:
            min_val = x
        if max_val is None or x > max_val:
            max_val = x
    return (min_val, max_val)
