# Beverage Bandits

import itertools
from collections import defaultdict, namedtuple

from utils import *

from .astar import *
from .world import Point, Thing, World

Outcome = namedtuple("Outcome", ["power", "turn", "outcome"])
TEST_OUTCOMES = [
    None,
    [
        Outcome(3, 47, 27730),
        Outcome(3, 37, 36334),
        Outcome(3, 46, 39514),
        Outcome(3, 35, 27755),
        Outcome(3, 54, 28944),
        Outcome(3, 20, 18740),
    ],
    [
        Outcome(15, 29, 4988),
        Outcome(None, None, None),
        Outcome(4, 33, 31284),
        Outcome(15, 37, 3478),
        Outcome(12, 39, 6474),
        Outcome(34, 30, 1140),
    ],
]


class GameEnd(BaseException):
    pass


class BattleThing(Thing):
    def is_creature(self):
        return False

    def is_alive(self):
        return False


class BattleWorld(World):
    def __init__(self, width, height):
        super().__init__(width, height)
        self.creatures = []

    def battle_outcome(self):
        for turn in itertools.count(1):
            try:
                for c in sorted(self.creatures, key=Creature.rank):
                    c.turn()  # OK if just killed; turn() checks for that
            except GameEnd:
                return (
                    turn - 1,
                    (turn - 1) * sum([c.hit_points for c in self.creatures]),
                )

    def set_elf_power(self, power):
        for c in self.creatures:
            if type(c) == Elf:
                c.attack_power = power

    def elves_win(self):
        self.remove_dead_creatures()  # just in case
        return len(self.creatures) > 0 and all([type(e) == Elf for e in self.creatures])

    def move(self, thing, from_loc, to_loc):
        """Moves thing from one loc to another.

        Does not change things's coords. Thing needs to do that.

        `from_loc` may be None, in which case we must be placing the thing
        for the first time.
        """
        if self.at(to_loc) is not None:
            raise Exception(
                f"to_loc {to_loc} already occupied by #{self.char_at(to_loc)}"
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
        for c in self.creatures[:]:
            if not c.is_alive():
                self.remove_dead_creature(c)

    def rank(self, x, y=None):
        if y is None:
            y = x.y
            x = x.x
        return y * self.height + x

    def print_map(self, turn=None):
        if turn:
            print()
            print(f"After {turn} rounds:")
        for y in range(self.height):
            hps = []
            for x in range(self.width):
                print(self.char_at(x, y), end="")
                thing = self.at(x, y)
                if thing and thing.is_creature():
                    hps.append(f" {thing.char}({thing.hit_points})")
            if hps:
                for hp_str in hps:
                    print(hp_str, end="")
            print()


class Wall(BattleThing):
    def __init__(self, world):
        super().__init__(world)
        self.char = "#"


class Creature(BattleThing):
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

    def rank(self):
        """'Reading order' value of this thing; lower == first."""
        return self.world.rank(self.loc)

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
        if self.loc not in possible_targets:
            self.move_towards_nearest(possible_targets)
        if self.loc in possible_targets:
            self.attack()

    def move_towards_nearest(self, locs):
        """Picks the closest loc and moves towards that."""
        astar_paths = defaultdict(list)  # key = dist, val = [path,...]
        for loc in locs:
            paths = self.astar_paths_to(loc)
            if paths:
                for p in paths:
                    astar_paths[len(p)].append(p)
        if len(astar_paths.keys()) == 0:
            return
        min_astar_dist = min(astar_paths.keys())
        closest_targets = [path[-1] for path in astar_paths[min_astar_dist]]
        target = min(closest_targets, key=lambda loc: self.world.rank(loc))
        paths_to_target = [
            path for path in astar_paths[min_astar_dist] if path[-1] == target
        ]
        target_path = min(paths_to_target, key=lambda path: self.world.rank(path[0]))
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
        return super().__str__() + f" hp:{self.hit_points} p:{self.attack_power}"


class Elf(Creature):
    def __init__(self, world):
        super().__init__(world)
        self.char = "E"


class Goblin(Creature):
    def __init__(self, world):
        super().__init__(world)
        self.char = "G"


def part1(env):
    world = _read_world(env)
    print(world.battle_outcome())


def part1_test(env):
    for i, expected in enumerate(TEST_OUTCOMES[1]):
        world = _read_world(env)
        turn, outcome = world.battle_outcome()
        answer = Outcome(3, turn, outcome)
        _compare_world_and_test_end(i, world)
        if answer != expected:
            print(f"test {i} expected {expected}, saw {answer}")
        else:
            print(f"test {i} ok")


def part2(testing=False):
    if testing:
    else:
        p, t, o = _min_guaranteed_win_battle_outcome(0, False)
        print(o)


def part2_test(env):
        for i, expected in enumerate(TEST_OUTCOMES[2]):
            if expected.power is None:
                continue
            answer = Outcome(*_min_guaranteed_win_battle_outcome(env))
            if answer != expected:
                print(f"test {i} expected {expected}, saw {answer}")
            else:
                print(f"test {i} ok")

def _min_guaranteed_win_battle_outcome(env):
    for elf_power in itertools.count(4):
        world = _read_world(env)
        world.set_elf_power(elf_power)
        turn, outcome = world.battle_outcome()
        if world.elves_win():
            return (elf_power, turn, outcome)
        elf_power += 1


def _read_world(env):
    lines = data_file_lines(env)
    world = BattleWorld(len(lines[0]), len(lines))
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


def _compare_world_and_test_end(i, world):
    fname = f"day15_{i}_test_end.txt"
    path = os.path.join(
        os.path.dirname(os.path.realpath(__file__)), "../../data/y2018", fname
    )
    lines = []
    with open(path, "r") as f:
        lines = [line for line in f.read().split("\n") if line]
    for y, line in enumerate(lines):
        for x, ch in enumerate(line):
            loc = Point(x, y)
            thing = world.at(loc)
            if thing:
                world_ch = thing.char
                if world_ch != ch:
                    raise Exception(f"expected {ch} at {loc} but saw {world_ch}")
            else:
                if ch != ".":
                    raise Exception(f"expected . at {loc} but saw {ch}")
