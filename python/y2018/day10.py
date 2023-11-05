# The Stars Align

import curses
import re
from collections import namedtuple
from itertools import groupby

from utils import *

LIGHT_REGEX = r"position=< *(-?\d+), *(-?\d+) *> velocity=< *(-?\d+), *(-?\d+) *>"

Point = namedtuple("Point", ["x", "y"])
Light = namedtuple("Light", ["pos", "vel"])


def part1(env):
    win = curses.initscr()
    curses.curs_set(0)

    lights = _read_lights(env)
    t = 0

    # Wait until lights are near each other
    while not _clustered(lights):
        lights = _move_lights(lights)
        t += 1

    while True:
        _print_lights(lights, t, win)
        _print_time(t, win)
        ch = chr(win.getch())
        if ch == "q":
            break
        lights = _move_lights(lights)
        if not _clustered(lights):
            break
        t += 1

    curses.curs_set(1)
    curses.endwin()


def part2(env):
    part1(env)


def _read_lights(env):
    lights = []
    for line in data_file_lines(env):
        match = re.search(LIGHT_REGEX, line)
        ints = [int(match.group(i + 1)) for i in range(4)]
        lights.append(Light(Point(ints[0], ints[1]), Point(ints[2], ints[3])))
    return lights


def _clustered(lights):
    min_x, min_y, max_x, max_y = _lights_minmax(lights)
    return (max_x - min_x) <= 100 and (max_y - min_y) <= 100


def _move_lights(lights):
    return [Light(Point(l.pos.x + l.vel.x, l.pos.y + l.vel.y), l.vel) for l in lights]


def _offset_lights(lights, row_offset, col_offset):
    return [
        Light(Point(l.pos.x + col_offset, l.pos.y + row_offset), l.vel) for l in lights
    ]


def _print_lights(lights, t, win):
    min_x, min_y, max_x, max_y = _lights_minmax(lights)
    prev_row, prev_col = 0, 0
    lights = _offset_lights(lights, -min_y, -min_x)
    win.clear()
    for l in lights:
        row = l.pos.y
        col = l.pos.x
        try:
            win.move(row, col)
        except:
            pass
        win.addch("*")
    win.refresh()


def _print_time(t, win):
    win.move(1, 1)
    win.addstr(str(t))


def _skip_to_row(old_row, new_row):
    while old_row < new_row:
        print()
        old_row += 1


def _skip_to_col(old_col, new_col):
    while old_col < new_col:
        print(".", end="")
        old_col += 1


def _lights_minmax(lights):
    min_x, min_y, max_x, max_y = None, None, None, None
    for l in lights:
        if min_x is None or l.pos.x < min_x:
            min_x = l.pos.x
        if max_x is None or l.pos.x > max_x:
            max_x = l.pos.x
        if min_y is None or l.pos.y < min_y:
            min_y = l.pos.y
        if max_y is None or l.pos.y > max_y:
            max_y = l.pos.y
    return (min_x, min_y, max_x, max_y)
