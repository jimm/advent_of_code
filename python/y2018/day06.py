# Chronal Coordinates
#


import collections

from utils import *

Point = collections.namedtuple("Point", ["x", "y"])
Coord = collections.namedtuple("Coord", ["id", "point"])
World = collections.namedtuple("World", ["coords", "width", "height", "cells"])


# - read coordinates
# - build world with minimal enclosing grid
# - find ownership of each point in the world
# - mark coordinates as finite/infinite
#   - for each edge point, see if moving one away from grid in the same
#     direction (up, down, left, right) only increases distance from all
#     coordinates
#     - if true, coordinate is infinite and you can skip all other edge
#       points owned by the same coordinate
# - return min area of all finite-bounded coordinates
#
# Run time: around 4 seconds
def part1(testing=False):
    world = _build_world(_read_coords(testing, 1))
    _claim_coordinates(world)
    if testing:
        _print_world(world)
    infinite_coords = _infinite_coords(world)
    finite_coords = set(world.coords) - set(infinite_coords)
    if testing:
        print(f"finite coords: {[_coord_name(c.id) for c in finite_coords]}")
        # remove infinite and re-print world
        _remove_infinite_coord_claims(world, infinite_coords)
        _print_world(world, empty_cell=" ")
    print(max([_coord_area(world, c) for c in finite_coords]))


# - read coordinates
# - build world with minimal enclosing grid
# - for every point, determine if it's within max dist
# - return number of those lower than max dist
#
# Run time: around 2.5 seconds
def part2(testing=False):
    world = _build_world(_read_coords(testing, 1))
    max_dist = testing and 32 or 10000
    close_point_count = 0
    for y in range(world.height):
        for x in range(world.width):
            if _total_dist_less_than(world, Point(x, y), max_dist):
                close_point_count += 1
    print(close_point_count)


def _manhattan_distance(p1, p2):
    return abs(p1.x - p2.x) + abs(p1.y - p2.y)


def _read_coords(testing, part_num):
    coords = []
    for i, line in enumerate(data_file_lines(6, part_num, testing)):
        x_str, y_str = line.split(", ")
        coords.append(Coord(i, Point(int(x_str), int(y_str))))
    return coords


def _build_world(coords):
    # This min/max calculation is inefficient, but fine fine given the size
    # of the input.
    vals = [c.point.x for c in coords]
    x_min = min(vals)
    x_max = max(vals)
    vals = [c.point.y for c in coords]
    y_min = min(vals)
    y_max = max(vals)
    world_width = x_max - x_min + 1
    world_height = y_max - y_min + 1
    cells = [list(range(world_width)) for _ in range(world_height)]
    shifted_coords = [
        Coord(c.id, Point(c.point.x - x_min, c.point.y - y_min)) for c in coords
    ]
    return World(shifted_coords, world_width, world_height, cells)


def _claim_coordinates(world):
    max_dist = world.width + world.height + 1
    for y in range(world.height):
        for x in range(world.width):
            closest_coord = _closest_coord(Point(x, y), world.coords, max_dist)
            if closest_coord:
                world.cells[y][x] = closest_coord.id
            else:
                world.cells[y][x] = None


def _closest_coord(p, coords, max_dist):
    dist_to_coords = collections.defaultdict(list)
    for c in coords:
        dist = _manhattan_distance(p, c.point)
        dist_to_coords[dist].append(c)
    min_coords = dist_to_coords[min(dist_to_coords.keys())]
    if len(min_coords) == 1:
        return min_coords[0]
    return None


# Horribly inefficient, but it works
def _print_world(world, empty_cell="."):
    for y in range(world.height):
        print(
            "".join(
                [_coord_name(world.cells[y][x], empty_cell) for x in range(world.width)]
            )
        )


def _coord_name(coord_id, empty_cell="."):
    if coord_id is None:
        return empty_cell
    if coord_id < 26:
        return chr(ord("A") + coord_id)
    return chr(ord("a") + coord_id - 26)


def _infinite_coords(world):
    infinite_coord_ids = []
    for y in [0, world.height - 1]:
        for x in range(world.width):
            id = world.cells[y][x]
            if id is None or id in infinite_coord_ids:
                continue
            delta = y > 0 and 1 or -1
            if _all_distances_increase(world.coords, Point(x, y), Point(x, y + delta)):
                infinite_coord_ids.append(id)
    for x in [0, world.width - 1]:
        for y in range(world.height):
            id = world.cells[y][x]
            if id is None or id in infinite_coord_ids:
                continue
            delta = x > 0 and 1 or -1
            if _all_distances_increase(world.coords, Point(x, y), Point(x + delta, y)):
                infinite_coord_ids.append(id)
    return [c for c in world.coords if c.id in infinite_coord_ids]


# Returns True if dist from all coords increases when moving from p0 to p1.
def _all_distances_increase(coords, p0, p1):
    for c in coords:
        if _manhattan_distance(c.point, p1) < _manhattan_distance(c.point, p0):
            return False
    return True


def _coord_area(world, coord):
    return sum(
        [
            1
            for y in range(world.height)
            for x in range(world.width)
            if world.cells[y][x] == coord.id
        ]
    )


def _remove_infinite_coord_claims(world, infinite_coords):
    ic_ids = [c.id for c in infinite_coords]
    for y in range(world.height):
        for x in range(world.width):
            if world.cells[y][x] in ic_ids:
                world.cells[y][x] = None


def _total_dist_less_than(world, p, max_dist):
    total_dists = 0
    for c in world.coords:
        total_dists += _manhattan_distance(p, c.point)
        if total_dists >= max_dist:
            return False
    return True
