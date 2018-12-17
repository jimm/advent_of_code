# Beverage Bandits

import collections

from utils import *

from .astar import *

# import pprint                   # DEBUG


Point = collections.namedtuple("Point", ["x", "y"])


TEST_OUTCOMES = [
    (47, 27730),
    (37, 36334),
    (46, 39514),
    (35, 27755),
    (54, 28944),
    (20, 18740),
]


class GameEnd(BaseException):
    pass


class World:
    instance = None

    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.map = [[None] * width for _ in range(height)]
        self.creatures = []
        World.instance = self

    def battle_outcome(self):
        turn = 0
        # self.print_map(turn)    # DEBUG
        # pause()                 # DEBUG
        while True:
            try:
                turn += 1
                for c in sorted(self.creatures, key=Creature.rank):
                    c.turn()  # OK if just killed; turn() checks for that
            except GameEnd:
                # self.print_map(turn)                    # DEBUG
                # print([str(c) for c in self.creatures]) # DEBUG
                return (
                    turn - 1,
                    (turn - 1) * sum([c.hit_points for c in self.creatures]),
                )
            # self.print_map(turn) # DEBUG
            # pause()              # DEBUG

    def move(self, thing, from_loc, to_loc):
        """Moves thing from one loc to another.

        Does not change things's coords. Thing needs to do that.

        `from_loc` may be None, in which case we must be placing the thing
        for the first time.
        """
        if self.at(to_loc) is not None:
            raise Exception(
                f"to_loc {to_loc} already occupied by #{self.at(to_loc).char}"
            )
        if from_loc:
            self.clear(from_loc)
        self.map[to_loc.y][to_loc.x] = thing
        thing.loc = to_loc

    def at(self, x, y=None):
        if y is None:
            y = x.y
            x = x.x
        return self.map[y][x]

    def is_empty(self, x, y=None):
        if y is None:
            y = x.y
            x = x.x
        return self.map[y][x] is None

    def clear(self, x, y=None):
        if y is None:
            y = x.y
            x = x.x
        self.map[y][x] = None

    def remove_dead_creature(self, c):
        self.clear(c.loc)
        self.creatures.remove(c)

    def remove_dead_creatures(self):
        for c in self.creatures:
            if not c.is_alive():
                self.remove_dead_creature(c)

    def print_map(self, turn):
        print()  # DEBUG
        print(f"After {turn} rounds:")  # DEBUG
        for y in range(self.height):
            hps = []
            for x in range(self.width):
                thing = self.at(x, y)
                print(f"{thing.char if thing else '.'}", end="")
                if thing and thing.is_creature():
                    hps.append(f" {thing.char}({thing.hit_points})")
            if hps:
                for hp_str in hps:
                    print(hp_str, end="")
            print()


class Thing:
    def __init__(self, world):
        self.world = world
        self.loc = (0, 0)
        self.char = "."

    def is_creature(self):
        return False

    def is_alive(self):
        return False

    def rank(self):
        """'Reading order' value of this thing; lower == first."""
        return self.loc.y * self.world.height + self.loc.x

    def __str__(self):
        return f"{self.char}@({self.loc.x},{self.loc.y})"


class Wall(Thing):
    def __init__(self, world):
        super().__init__(world)
        self.char = "#"


class Creature(Thing):
    def __init__(self, world):
        super().__init__(world)
        self.attack_power = 3
        self.hit_points = 200

    def is_creature(self):
        return True

    def is_alive(self):
        return self.hit_points > 0

    def is_enemy(self, thing):
        return thing and thing.is_alive() and type(thing) != type(self)

    def turn(self):
        if not self.is_alive():  # was just killed
            return
        enemies = [c for c in self.world.creatures if self.is_enemy(c)]
        if not enemies:
            raise GameEnd("battle is over!")

        possible_targets = [
            loc
            for loc in set(flatten([c.adjacent() for c in enemies]))
            if self.world.is_empty(loc.x, loc.y) or self.world.at(loc.x, loc.y) == self
        ]
        # print(f"{str(self)} possible targets {possible_targets}") # DEBUG
        if self.loc not in possible_targets:
            self.move_towards_nearest(possible_targets)
        if self.loc in possible_targets:
            self.attack()

    def move_towards_nearest(self, locs):
        """Picks the closest loc and moves towards that."""
        astar_paths = collections.defaultdict(list)  # key = dist, val = [path,...]
        for loc in locs:
            paths = self.astar_paths_to(loc)
            if paths:
                for p in paths:
                    astar_paths[len(p)].append(p)
        if len(astar_paths.keys()) == 0:
            return
        min_astar_dist = min(astar_paths.keys())
        # print(f"{str(self)} min_astar_dist {min_astar_dist}, all paths:") # DEBUG
        # pprint.pprint(astar_paths) # DEBUG
        target_path = min(
            astar_paths[min_astar_dist],
            key=lambda path: path[0].y * self.world.height + path[0].x,
        )
        self.move_along_path(target_path)

    def astar_paths_to(self, loc):
        blocker = Thing(self.world)
        blocked = []
        paths = []
        astar_path = astar(self.world, self.loc, loc)
        while astar_path:
            # path is in reverse of desired order
            path = astar_path[::-1]
            paths.append(path)
            block_loc = path[0]
            blocked.append(block_loc)
            self.world.map[block_loc.y][block_loc.x] = blocker
            astar_path = astar(self.world, self.loc, loc)
        for block_loc in blocked:
            self.world.map[block_loc.y][block_loc.x] = None
        return paths

    def move_along_path(self, target_path):
        loc = target_path[0]
        # print(f"{str(self)} moving to {loc} along path {target_path}") # DEBUG
        self.world.move(self, self.loc, loc)  # Modifies self.loc

    def attack(self):
        attackables = []
        for loc in self.adjacent():
            thing = self.world.at(loc)
            if self.is_enemy(thing):
                attackables.append(thing)
        if not attackables:
            return
        min_hp = min([a.hit_points for a in attackables])
        target = min(
            [a for a in attackables if a.hit_points == min_hp], key=Creature.rank
        )
        target.take_damage(self.attack_power)

    def adjacent(self):
        """Returns locations of all adjacent squares, no matter their contents.

        The locs are returned in rank order.
        """
        return [
            loc
            for loc in [
                Point(self.loc.x, self.loc.y - 1),
                Point(self.loc.x - 1, self.loc.y),
                Point(self.loc.x + 1, self.loc.y),
                Point(self.loc.x, self.loc.y + 1),
            ]
        ]

    def take_damage(self, hp):
        self.hit_points -= hp
        if not self.is_alive():
            self.world.remove_dead_creature(self)

    def __str__(self):
        return super().__str__() + f" hp {self.hit_points}"


class Elf(Creature):
    def __init__(self, world):
        super().__init__(world)
        self.char = "E"


class Goblin(Creature):
    def __init__(self, world):
        super().__init__(world)
        self.char = "G"


def part1(testing=False):
    if testing:
        ok = True
        # 0th is from puzzle description
        for i in range(len(TEST_OUTCOMES)):
            world = _read_world(i, testing)
            turn_and_outcome = world.battle_outcome()
            if turn_and_outcome != TEST_OUTCOMES[i]:
                print(f"test {i} expected {TEST_OUTCOMES[i]}, saw {turn_and_outcome}")
                ok = False
        if ok:
            print("ok")
    else:
        world = _read_world(0, False)
        print(world.battle_outcome())


def part2(testing=False):
    lines = data_file_lines(15, 2, testing)


def _read_world(test_part, testing):
    lines = data_file_lines(15, test_part, testing)
    world = World(len(lines[0]), len(lines))
    for y, line in enumerate(lines):
        for x, ch in enumerate(line):
            loc = Point(x, y)
            if ch == "#":
                world.move(Wall(world), None, loc)
            elif ch == "E":
                creature = Elf(world)
                world.move(creature, None, loc)
                world.creatures.append(creature)
            elif ch == "G":
                creature = Goblin(world)
                world.move(creature, None, loc)
                world.creatures.append(creature)
    return world
