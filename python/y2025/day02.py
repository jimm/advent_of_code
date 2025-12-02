# ================ Gift Shop ================

import testing
import utils


def _do_part(ctx, lines, invalid_func):
    range_strs = lines[0].split(",")
    ranges = [
        range(int(min_val), int(max_val) + 1)
        for min_val, max_val in [r.split("-") for r in range_strs]
    ]
    sum_invalid = 0
    for r in ranges:
        for i in r:
            if invalid_func(i):
                sum_invalid += i
    return sum_invalid


def part1(ctx, lines=None):
    if not lines:
        lines = testing.data_file_lines(ctx)
    return _do_part(ctx, lines, _invalid1)


def _invalid1(i):
    s = str(i)
    l = len(s)
    if l % 2 != 0:
        return False
    half = int(len(s) / 2)
    return s[:half] == s[half:]


def part2(ctx, lines=None):
    if not lines:
        lines = testing.data_file_lines(ctx)
    return _do_part(ctx, lines, _invalid2)


def _invalid2(i):
    s = str(i)
    l = len(s)
    half = int(len(s) / 2)
    for i in range(1, half + 1):
        # quick validity checks: first or last letters of substring
        # don't match the last possible match
        if s[0] != s[-i] or s[i - 1] != s[-1]:
            continue  # can't be invalid

        pieces = list(utils.chunks(s, i))
        substr = pieces[0]
        if all(substr == pieces[i] for i in range(1, len(pieces))):
            return True
    return False
