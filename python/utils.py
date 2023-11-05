import datetime
import itertools
import os.path
import sys


def read_data_file(year=None, day=None, part_num=1, testing=False):
    """Returns the contents of a data file as a string."""
    now = datetime.datetime.today()
    if not day:
        day = now.day
    if not year:
        year = now.year
    fname = f"day{'%02d' % day}_{part_num}"
    if testing:
        fname += "_test"
    fname += ".txt"
    path = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), f"../data/y{year}", fname
    )
    with open(path, "r") as f:
        return f.read()


def run_one_test(expected, func):
    """Runs `func`, passing in `expected`, and prints success or failure.

    `run_chunk_tests` does not call this method.
    """
    answer = func(expected)
    if answer == expected:
        print(".")
        print("ok")
    else:
        print("F")
        print(f"error: expected {expected}, got {answer}")


def run_chunk_tests(part_number, func, year=None, day=None):
    """Runs tests and compares with expected answers.

    Given an optional part number, reads each test chunk and yields the
    expected value as a string and the data lines. The block must return a
    (boolean, answer) pair or (boolean, answer, expected) triplet. Prints
    success or failure for all the tests.

    A test chunk starts with a line starting with '# <expected>' and ends at
    the next such line or the end of the file.
    """
    errors = []
    for expected, lines in test_chunks(part_number, year=year, day=day):
        result = func(expected, lines)
        if len(result) == 2:
            ok, answer = result
            optional_expected = None
        else:
            ok, answer, optional_expected = result
        if ok:
            print(".", end="")
        else:
            print("F", end="")
            errors.append((optional_expected or expected, answer))
    print("")
    if not errors:
        print("ok")
    else:
        for expect, answer in errors:
            print(f"expected {expected}, got {answer}")


def data_file_lines(
    year=None, day=None, part_num=1, preserve_blank_lines=False, testing=False
):
    """Returns lines from a data file with blank lines skipped optionally.

    If preserve_blank_lines is False (the default), returns a list of all
    non-blank lines. If it is True, returns a list of lists.
    """
    lines = read_data_file(year, day, part_num, testing).split("\n")
    if not preserve_blank_lines:
        return [line for line in lines if line]

    groups = []
    for k, g in itertools.groupby(lines, key=lambda x: x == ""):
        groups.append(list(g))
    return [g for g in groups if g[0]]


def test_chunks(part_number, year=None, day=None):
    """Returns tuples like (expected, [lines...]).

    Many times test data files have multiple tests. (These are usually files
    that I've created based on multiple test cases provided by the problem
    description and/or my needs.) The first line will start with '#' and the
    data/input for the test is the following lines up to the next '#' or
    EOF. This method returns a list of two-element lists where the first
    element is the '#' line, minus the '#' and any leading whitespace, and
    the second element is the array of strings contains the data lines for
    that test.
    """
    chunks = []
    chunk_index = -1
    for line in data_file_lines(year=year, day=day, part_num=part_number, testing=True):
        if line[0] == "#":
            chunk_index += 1
            expected = line[1:].strip()
            chunks.append((expected, []))
        elif chunk_index >= 0:
            expected, lines = chunks[chunk_index]
            lines.append(line)
            chunks[chunk_index] = (expected, lines)
    return chunks


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
