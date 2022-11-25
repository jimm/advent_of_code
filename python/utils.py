import datetime
import itertools
import os.path
import sys


def file_year(path):
    """Returns year XXXX as int from file path .../yXXXX/dayYY.py"""
    return int(os.path.basename(os.path.dirname(path))[1:])


def file_day(path):
    """Returns day YY as int from file path .../yXXXX/dayYY.py"""
    date_str = os.path.basename(path)[3:5]
    if date_str[0] == "0":
        date_str = date_str[1:]
    return int(date_str)


def read_data_file(year=None, day=None, part_num=1, testing=False):
    """Returns the contents of a data file as a string."""
    now = datetime.datetime.today()
    if not day:
        day = now.day
    if not year:
        year = now.year
    fname = f"day{'%02d' % day}"
    if testing:
        fname += f"_{part_num}_test"
    fname += ".txt"
    path = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), f"../data/y{year}", fname
    )
    with open(path, "r") as f:
        return f.read()


def data_file_lines(year=None, day=None, part_num=1, testing=False):
    """Returns a list of lines from a data file with blank lines skipped."""
    return [
        line
        for line in read_data_file(year, day, part_num, testing).split("\n")
        if line
    ]


def minmax(xs):
    """Given an iterable, returns a (min, max) tuple."""
    min_val = None
    max_val = None
    for x in xs:
        if min_val is None or x < min_val:
            min_val = x
        if max_val is None or x > max_val:
            max_val = x
    return (min_val, max_val)


def flatten(list_of_lists):
    """Flattens one level."""
    return itertools.chain.from_iterable(list_of_lists)


pause_continue = False


def pause():
    """Writes a message and waits for input.

    Does nothing if the global `pause_continue` is True."""
    global pause_continue
    if pause_continue:
        return
    line = input(
        "Paused. 'q' to quit, 'c' to continue without pausing, anything else to step."
    )
    if line:
        if line[0] == "q":
            exit(0)
        if line[0] == "c":
            pause_continue = True
