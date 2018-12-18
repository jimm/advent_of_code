# 2D generic world

import collections

Point = collections.namedtuple("Point", ["x", "y"])


class Thing:
    def __init__(self, world, loc=Point(0, 0)):
        self.world = world
        self.loc = loc
        self.char = "."

    def move(self, x, y=None, ignore_out_of_bounds=False):
        p = Point(x, y) if y else x
        self.world.move(
            self,
            self.loc,
            p,
            ensure_empty=False,
            ignore_out_of_bounds=ignore_out_of_bounds,
        )

    def __str__(self):
        return f"{self.char}@({self.loc.x},{self.loc.y})"


class World:
    """A 2D world. One thing at a time in each location."""

    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.map = [[None] * width for _ in range(height)]
        self.turn = 0

    def move(
        self, thing, from_loc, to_loc, ensure_empty=False, ignore_out_of_bounds=False
    ):
        """Moves thing from one loc to another.

        Does not change things's coords. Thing needs to do that.

        `from_loc` may be None, in which case we must be placing the thing
        for the first time.
        """
        if ensure_empty and self.at(to_loc) is not None:
            raise Exception(
                f"to_loc {to_loc} already occupied by #{self.char_at(to_loc)}"
            )
        if from_loc:
            self.clear(from_loc)
        if getattr(thing, "loc", None):
            thing.loc = to_loc
        if ignore_out_of_bounds and not self.in_bounds(to_loc):
            return
        self.map[to_loc.y][to_loc.x] = thing

    def at(self, x, y=None):
        if y is None:
            y = x.y
            x = x.x
        if not self.in_bounds(x, y):
            return None
        return self.map[y][x]

    def char_at(self, x, y=None):
        thing = self.at(x, y)
        if thing is None:
            return "."
        elif type(thing) == str:
            return thing[0]
        elif isinstance(thing, Thing):
            return thing.char
        else:
            return "?"

    def is_empty(self, x, y=None):
        if y is None:
            y = x.y
            x = x.x
        return self.map[y][x] is None

    def clear(self, x, y=None):
        if y is None:
            y = x.y
            x = x.x
        if self.in_bounds(x, y):
            self.map[y][x] = None

    def in_bounds(self, x, y=None):
        if y is None:
            y = x.y
            x = x.x
        return y >= 0 and y < self.height and x >= 0 and x < self.width

    def print_map(self, turn=None):
        if turn:
            print()
            print(f"After {turn} turns:")
        for y in range(self.height):
            for x in range(self.width):
                print(self.char_at(x, y), end="")
            print()
