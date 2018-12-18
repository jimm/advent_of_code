# Settlers of The North Pole

import collections

from utils import *

from .world import Point, Thing, World


class Forest(World):
    def __init__(self, width, height):
        super().__init__(width, height)
        self.t = 0

    def score_after_n_turns(self, n):
        previous_map_strings = []
        for _ in range(n):
            self.pass_time()
            save_str = self._map_save()
            if save_str in previous_map_strings:
                idx = previous_map_strings.index(save_str)
                cycle = previous_map_strings[idx:]
                save_str = cycle[(n - self.t) % len(cycle)]
                num_trees = len([1 for ch in save_str if ch == "|"])
                num_yards = len([1 for ch in save_str if ch == "#"])
                return num_trees * num_yards
            else:
                previous_map_strings.append(self._map_save())
        num_trees = len(
            [
                1
                for y in range(self.height)
                for x in range(self.width)
                if self.at(x, y) == "|"
            ]
        )
        num_yards = len(
            [
                1
                for y in range(self.height)
                for x in range(self.width)
                if self.at(x, y) == "#"
            ]
        )
        return num_trees * num_yards

    def pass_time(self):
        new_map = [[None] * self.width for _ in range(self.height)]
        for y in range(self.height):
            for x in range(self.width):
                self._apply_rules(x, y, new_map)
        self.map = new_map
        self.t += 1

    def _apply_rules(self, x, y, new_map):
        counts = collections.defaultdict(int)
        counts[self.at(x - 1, y - 1)] += 1
        counts[self.at(x, y - 1)] += 1
        counts[self.at(x + 1, y - 1)] += 1
        counts[self.at(x - 1, y)] += 1
        counts[self.at(x + 1, y)] += 1
        counts[self.at(x - 1, y + 1)] += 1
        counts[self.at(x, y + 1)] += 1
        counts[self.at(x + 1, y + 1)] += 1

        curr_ch = self.at(x, y)
        new_map[y][x] = curr_ch
        if curr_ch == ".":
            if counts["|"] >= 3:
                new_map[y][x] = "|"
            return
        if curr_ch == "|":
            if counts["#"] >= 3:
                new_map[y][x] = "#"
            return
        if curr_ch == "#":
            if counts["#"] == 0 or counts["|"] == 0:
                new_map[y][x] = "."

    def _map_save(self):
        return "".join(
            [self.at(x, y) for y in range(self.width) for x in range(self.height)]
        )


def part1(testing=False):
    world = _read_world(1, testing)
    print(world.score_after_n_turns(10))


def part2(testing=False):
    world = _read_world(1, testing)
    # It cycles (I can see it if I print out world map at each turn)
    print(world.score_after_n_turns(1_000_000_000))


def _read_world(part_num, testing):
    lines = data_file_lines(18, part_num, testing)
    world = Forest(len(lines[0]), len(lines))
    for y, line in enumerate(lines):
        for x, ch in enumerate(line):
            world.move(ch, None, Point(x, y))
    return world
