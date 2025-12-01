# ================ Secret Entrance ================

import testing


def part1(ctx, lines=None):
    if not lines:
        lines = testing.data_file_lines(ctx)
    pos = 50
    num_zeroes = 0
    for line in lines:
        pos, _ = _move(pos, line)
        if pos == 0:
            num_zeroes += 1
    return num_zeroes


def part2(ctx, lines=None):
    if not lines:
        lines = testing.data_file_lines(ctx)
    pos = 50
    num_zeroes = 0
    for line in lines:
        pos, num_zeroes_seen = _move(pos, line)
        num_zeroes += num_zeroes_seen
    return num_zeroes


def _move(pos, line):
    direction = line[0]
    num = int(line[1:])
    delta = 1 if direction == "R" else -1
    num_zeroes = 0
    for _ in range(num):
        pos = (pos + delta) % 100
        if pos == 0:
            num_zeroes += 1
    pos = pos % 100
    return (pos, num_zeroes)
