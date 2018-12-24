# Experimental Emergency Teleportation

import re
from collections import namedtuple

from utils import *

Point3 = namedtuple("Point3", ["x", "y", "z"])
Bot = namedtuple("Bot", ["loc", "r"])


def part1(testing=False):
    bots = _read_nanobots(1, testing)
    strongest = max(bots, key=lambda b: b.r)
    if testing:
        print(strongest)
    print(len([b for b in bots if _mdist(strongest.loc, b.loc) <= strongest.r]))


def part2(testing=False):
    bots = _read_nanobots(2, testing)
    max_num = -1
    max_coords = None
    min_x, max_x = minmax(b.loc.x for b in bots)
    min_y, max_y = minmax(b.loc.y for b in bots)
    min_z, max_z = minmax(b.loc.z for b in bots)

    # let's try brute force
    for x in range(min_x, max_x + 1):
        for y in range(min_y, max_y + 1):
            for z in range(min_z, max_z + 1):
                loc = Point3(x, y, z)
                num_in_range = _num_in_range(loc, bots)
                if num_in_range > max_num:
                    max_num = num_in_range
                    max_coords = [loc]
                elif num_in_range == max_num:
                    max_coords.append(loc)
    closest = min(max_coords, key=lambda l: _mlen(l))
    print(_mlen(closest))


def _read_nanobots(part_num, testing):
    bots = []
    for line in data_file_lines(23, part_num, testing):
        m = re.match(r"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)", line)
        bots.append(Bot(Point3(int(m[1]), int(m[2]), int(m[3])), int(m[4])))
    return bots


def _mdist(loc1, loc2):
    return abs(loc1.x - loc2.x) + abs(loc1.y - loc2.y) + abs(loc1.z - loc2.z)


def _mlen(loc):
    return abs(loc.x) + abs(loc.y) + abs(loc.z)


def _num_in_range(loc, bots):
    return len([b for b in bots if _mdist(loc, b.loc) <= b.r])
