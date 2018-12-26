# Four-Dimensional Adventure

from collections import namedtuple

from utils import *

Point4D = namedtuple("Point4D", ["x", "y", "z", "t"])

# test num, expected constellation count
tests = {1: 4, 2: 3, 3: 8}


def part1(testing=False):
    if testing:
        ok = True
        for i in tests:
            stars = _read_stars(i, testing)
            constellations = _find_constellations(stars)
            lc = len(constellations)
            if lc != tests[i]:
                print(f"test {i} expected {tests[i]} constellations, found {lc}")
                ok = False
        if ok:
            print("ok")
    else:
        stars = _read_stars(1, testing)
        constellations = _find_constellations(stars)
        print(len(constellations))


def part2(testing=False):
    lines = data_file_lines(25, 2, testing)


def _read_stars(i, testing):
    return [_read_star(line) for line in data_file_lines(25, i, testing)]


def _read_star(line):
    return Point4D(*[int(num) for num in line.split(",")])


def _find_constellations(stars):
    constellations = []
    for star in stars:
        belongs_to = []
        for constellation in constellations:
            if _belongs_to_constellation(star, constellation):
                belongs_to.append(constellation)
        if len(belongs_to) == 0:
            constellations.append(set([star]))
        elif len(belongs_to) == 1:
            belongs_to[0].add(star)
        else:
            merge_with = belongs_to[0]
            merge_with.add(star)
            for c in belongs_to[1:]:
                merge_with.update(c)
                constellations.remove(c)
    return constellations


def _belongs_to_constellation(star, constellation):
    for cs in constellation:
        if _close_enough(star, cs):
            return True
    return False


def _close_enough(p1, p2):
    dist = abs(p1.x - p2.x) + abs(p1.y - p2.y) + abs(p1.z - p2.z) + abs(p1.t - p2.t)
    return dist <= 3
