# Reservoir Research

import re

from utils import *

from .world import Point, Thing, World


class WaterWorld(World):
    def __init__(self, width, height):
        super().__init__(width, height)
        self.min_y = 9999
        self.max_y = 0
        self.min_x = 9999
        self.max_x = 0
        self.flowing_water = []
        self.resting_water = []
        self.faucet = None

    def move(
        self, thing, from_loc, to_loc, ensure_empty=False, ignore_out_of_bounds=False
    ):
        super().move(
            thing,
            from_loc,
            to_loc,
            ensure_empty=ensure_empty,
            ignore_out_of_bounds=ignore_out_of_bounds,
        )
        if type(thing) == Clay:
            if to_loc.y < self.min_y:
                self.min_y = to_loc.y
            if to_loc.y > self.max_y:
                self.max_y = to_loc.y
            if to_loc.x - 1 < self.min_x:
                self.min_x = to_loc.x - 1
            if to_loc.x + 1 > self.max_x:
                self.max_x = to_loc.x + 1

    def water_stopped_flowing(self, water):
        self.resting_water.append(water)
        self.flowing_water.remove(water)

    def reachable_tiles(self):
        turn = 0
        prev_count = -1
        while True:
            for w in self.flowing_water[:]:
                w.flow()
            w = Water(self)
            self.move(w, None, Point(self.faucet.loc.x, self.faucet.loc.y + 1))
            self.flowing_water.append(w)
            self.print_map(turn)  # DEBUG
            pause()  # DEBUG
            count = len(self.resting_water) + len(
                [
                    w
                    for w in self.flowing_water
                    if w.loc.y in range(self.min_y, self.max_y + 1)
                ]
            )
            if count == prev_count:
                return count
            prev_count = count
            turn += 1

    def print_map(self, turn=None):
        if turn:
            print()
            print(f"After {turn} turns:")
        for y in range(self.min_y, self.max_y + 1):
            for x in range(self.min_x, self.max_x + 1):
                print(self.char_at(x, y), end="")
            print()


class Clay(Thing):
    def __init__(self, world):
        super().__init__(world)
        self.char = "#"


# TODO when drops water, start y is one less than min y of any clay
class Faucet(Thing):
    def __init__(self, world):
        super().__init__(world)
        self.char = "+"

    def flow(self):
        pass


class Water(Thing):
    def __init__(self, world):
        super().__init__(world)
        self.char = "|"
        self.can_flow = True

    # If new water created, returns it
    def flow(self):
        if not self.can_flow:
            return

        start_loc = Point(self.loc.x, self.loc.y)

        p = Point(self.loc.x, self.loc.y + 1)
        if self.world.at(p) is None:
            self.move(p, ignore_out_of_bounds=True)
            self.char = "|"
            return
        self.char = "~"

        # if below left and right are both already taken, can't move
        if (
            type(self.world.at(start_loc.x - 1, start_loc.y + 1)) == Water
            and type(self.world.at(start_loc.x + 1, start_loc.y + 1)) == Water
        ):
            self.can_flow = False
            self.world.water_stopped_flowing(self)
            return

        moved_left = False
        p = Point(start_loc.x - 1, start_loc.y)
        if self.world.at(p) is None:
            self.move(p, ignore_out_of_bounds=True)
            moved_left = True

        p = Point(start_loc.x + 1, start_loc.y)
        if self.world.at(p) is None:
            if moved_left:
                w = Water(self.world)
                self.world.move(w, None, p)
            else:
                self.move(p, ignore_out_of_bounds=True)
        else:
            if not moved_left:
                self.can_flow = False


def part1(env):
    world = _read_world(env)
    if env.test:
        world.print_map()
    print(world.reachable_tiles())


def part2(env):
    world = _read_world(env)


def _read_world(env):
    soundings = []
    for line in data_file_lines(env):
        print(line)  # DEBUG
        m = re.match(r"x=(\d+), y=(\d+)\.\.(\d+)", line)
        if m:
            soundings.append((int(m[1]), int(m[1]), int(m[2]), int(m[3])))
        else:
            m = re.match(r"y=(\d+), x=(\d+)\.\.(\d+)", line)
            soundings.append((int(m[2]), int(m[3]), int(m[1]), int(m[1])))
    print(soundings)  # DEBUG
    x_min = min(soundings, key=lambda s: s[0])[0] - 1
    x_max = max(soundings, key=lambda s: s[1])[1] + 1
    y_min = min(soundings, key=lambda s: s[2])[2] - 1
    y_max = max(soundings, key=lambda s: s[3])[3] + 1
    world = WaterWorld(x_max + 1, y_max + 1)
    for s in soundings:
        for y in range(s[2], s[3] + 1):
            for x in range(s[0], s[1] + 1):
                world.move(Clay(world), None, Point(x, y))
    faucet = Faucet(world)
    world.move(faucet, None, Point(500, 0))
    world.flowing_water.append(faucet)
    world.faucet = faucet
    return world
