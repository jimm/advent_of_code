import datetime
import itertools
import os.path
import sys


def _data_file_path(ctx, part_number):
    """Returns contents of data file as a string."""
    fname = f"day{'%02d' % ctx.day}"
    if part_number:
        fname += f"_{part_number}"
    if ctx.test:
        fname += "_test"
    fname += ".txt"
    return os.path.join(
        os.path.dirname(os.path.realpath(__file__)),
        f"../data/y{ctx.year}",
        fname,
    )


def read_data_file(ctx):
    """Returns the contents of a data file as a string.

    Tries a few different file names. First we try "dayXX_PART.txt" then, if
    that does not exist and PART is > 1 then we try with part == 1. Finally,
    we try without any part number at all because both parts share the same
    data file.

    In all cases, if `ctx.test` is True we append "_test" to the file name
    before the extension. For example, on day 3 with ctx.test == True the
    file name would be "day03_PART_test.txt" or "day03_test.txt".

    If there are no test data files, print an error message and exit.
    """
    path = _data_file_path(ctx, ctx.part_number)  # with part number
    if not os.path.isfile(path) and ctx.part_number != 1:
        path = _data_file_path(ctx, 1)  # not part 1, try part 1
    if not os.path.isfile(path):
        path = _data_file_path(ctx, None)  # try without any part number
    if not os.path.isfile(path):
        raise FileNotFoundError(
            f"no test data file found for part {ctx.part_number}"
        )
    with open(path, "r") as f:
        return f.read()


def run_chunk_tests(ctx, func):
    """Runs tests and compares with expected answers.

    Given an optional part number, reads each test chunk and yields the
    expected value as a string and the data lines. The block must return a
    (boolean, answer) pair or (boolean, answer, expected) triplet. Prints
    success or failure for all the tests.

    A test chunk starts with a line starting with '# <expected>[,<expected]]'
    (any beginning char will do, actually), and ends at the next such line
    or the end of the file.

    If there are no test data files, print an error message and exit.
    """
    errors = []
    for expected_vals, lines in test_chunks(ctx):
        expected = expected_vals.split(",")[ctx.part_number - 1]
        answer = func(ctx, lines)
        if str(answer) == expected:
            print(".", end="")
        else:
            print("F", end="")
            errors.append((expected, answer))
    print("")
    if not errors:
        print("ok")
    else:
        for expected, answer in errors:
            print(f"expected {expected}, got {answer}")


def data_file_lines(ctx, preserve_blank_lines=False):
    """Returns lines from a data file with blank lines skipped optionally.

    If preserve_blank_lines is False (the default), returns a list of all
    non-blank lines. If it is True, returns a list of lists.

    If there are no test data files, print an error message and exit.
    """
    lines = read_data_file(ctx).split("\n")
    if not preserve_blank_lines:
        return [line for line in lines if line]

    groups = []
    for k, g in itertools.groupby(lines, key=lambda x: x == ""):
        groups.append(list(g))
    return [g for g in groups if g[0]]


def test_chunks(ctx):
    """Returns tuples like (expected, [lines...]).

    Many times test data files have multiple tests. (These are usually files
    that I've created based on multiple test cases provided by the problem
    description and/or my needs.) The first line will start with any
    character (for example, '#' or ';', but any character will do) and the
    remainder of the lines is a comma-separated list of expected values for
    the first part and optionally the second part.. The data/input for the
    test is the following lines up to the next delimiter or EOF.

    This function returns a list of two-element lists where the first
    element is the expected line line, minus the delimter and any leading
    whitespace, and the second element is the array of strings contains the
    data lines for that test.

    If there are no test data files, print an error message and exit.
    """
    chunks = []
    expected_line_char = None
    first_line = True
    for line in data_file_lines(ctx):
        if first_line:
            expected_line_char = line[0]
            first_line = False
        if line[0] == expected_line_char:
            # remove last empty line of previous chunk
            if chunks and not chunks[-1][-1]:
                chunks[-1] = (chunks[-1][0], chunks[-1][1][:-1])

            expected = line[1:].strip()
            chunks.append((expected, []))
        elif chunks:
            chunks[-1][1].append(line)
    return chunks
