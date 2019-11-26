# Mode Maze

import re
from collections import namedtuple

from utils import *

from .world import Point

Region = namedtuple("Region", ["geo_index", "erosion_level", "type_val"])

part1_expected_regions = {
    Point(0, 0): Region(0, 510, 0),
    Point(1, 0): Region(16807, 17317, 1),
    Point(0, 1): Region(48271, 8415, 0),
    Point(1, 1): Region(145722555, 1805, 2),
    Point(10, 10): Region(0, 510, 0),
}


def part1(testing=False):
    depth, target_loc = _read_data(1, testing)
    if testing:
        print((depth, target_loc))
    underground_map = _create_map(depth, target_loc)
    if testing:
        for loc, r in part1_expected_regions.items():
            map_r = underground_map[loc.y][loc.x]
            if map_r != r:
                print("error: at {loc} expected {r} but saw {map_r}")
    print(
        sum(
            underground_map[y][x].type_val
            for y in range(target_loc.y + 1)
            for x in range(target_loc.x + 1)
        )
    )


def part2(testing=False):
    depth, target_loc = _read_data(2, testing)


def _read_data(part_num, testing):
    lines = data_file_lines(2018, 22, part_num, testing)
    m = re.match(r"depth: (\d+)", lines[0])
    depth = int(m[1])
    m = re.match(r"target: (\d+),\s*(\d+)", lines[1])
    target_loc = Point(int(m[1]), int(m[2]))
    return (depth, target_loc)


# map cells contain (geo_index, erosion_level) tuples
def _create_map(depth, target_loc):
    underground_map = []
    for y in range(target_loc.y + 1):
        underground_map.append([])
        for x in range(target_loc.x + 1):
            underground_map[y] = [0] * target_loc.x
    underground_map = [([0] * (target_loc.x + 1)) for y in range(target_loc.y + 1)]
    for y in range(target_loc.y + 1):
        for x in range(target_loc.x + 1):
            underground_map[y][x] = _calc_map_tuple(
                depth, underground_map, target_loc, Point(x, y)
            )
    return underground_map


def _calc_map_tuple(depth, underground_map, target_loc, loc):
    geo_index = 0
    if (loc.x == 0 and loc.y == 0) or (loc.x == target_loc.x and loc.y == target_loc.y):
        geo_index = 0
    elif loc.y == 0:
        geo_index = loc.x * 16807
    elif loc.x == 0:
        geo_index = loc.y * 48271
    else:
        um1 = underground_map[loc.y][loc.x - 1]
        if um1 is None:
            um1 = _calc_map_tuple(
                depth, underground_map, target_loc, Point(loc.x - 1, loc.y)
            )
        um2 = underground_map[loc.y - 1][loc.x]
        if um2 is None:
            um2 = _calc_map_tuple(
                depth, underground_map, target_loc, Point(loc.x, loc.y - 1)
            )
        geo_index = um1.erosion_level * um2.erosion_level

    erosion_level = (geo_index + depth) % 20183
    type_val = erosion_level % 3
    return Region(geo_index, erosion_level, type_val)
