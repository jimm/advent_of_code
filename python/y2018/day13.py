# Mine Cart Madness
from enum import Enum, auto
from itertools import cycle

from utils import *


class Track(Enum):
    BLANK = auto()
    VERTICAL = auto()
    HORIZONTAL = auto()
    CURVE_NE = auto()
    CURVE_NW = auto()
    INTERSECTION = auto()


# Also encode movement delta
class Direction(Enum):
    UP = (0, -1)
    DOWN = (0, 1)
    LEFT = (-1, 0)
    RIGHT = (1, 0)


class Turn(Enum):
    LEFT = 0
    STRAIGHT = 1
    RIGHT = 2

    @classmethod
    def turn_cycle(cls):
        return cycle(cls)  # [Turn.LEFT, Turn.STRAIGHT, Turn.RIGHT])


curve_new_dirs = {
    Track.CURVE_NE: {
        Direction.UP: Direction.RIGHT,
        Direction.DOWN: Direction.LEFT,
        Direction.LEFT: Direction.DOWN,
        Direction.RIGHT: Direction.UP,
    },
    Track.CURVE_NW: {
        Direction.UP: Direction.LEFT,
        Direction.DOWN: Direction.RIGHT,
        Direction.LEFT: Direction.UP,
        Direction.RIGHT: Direction.DOWN,
    },
}

turn_new_dirs = {
    Turn.LEFT: {
        Direction.UP: Direction.LEFT,
        Direction.DOWN: Direction.RIGHT,
        Direction.LEFT: Direction.DOWN,
        Direction.RIGHT: Direction.UP,
    },
    Turn.RIGHT: {
        Direction.UP: Direction.RIGHT,
        Direction.DOWN: Direction.LEFT,
        Direction.LEFT: Direction.UP,
        Direction.RIGHT: Direction.DOWN,
    },
    Turn.STRAIGHT: {
        Direction.UP: Direction.UP,
        Direction.DOWN: Direction.DOWN,
        Direction.LEFT: Direction.LEFT,
        Direction.RIGHT: Direction.RIGHT,
    },
}


class Cart(object):
    TURN_LEFT = 0
    TURN_STRAIGHT = 1
    TURN_RIGHT = 2

    def __init__(self, x, y, dir):
        self.x = x
        self.y = y
        self.dir = dir
        self.turn_cycle = Turn.turn_cycle()
        self.next_turn = next(self.turn_cycle)

    def move(self, track, carts):
        dx, dy = self.dir.value
        self.x += dx
        self.y += dy
        new_track_piece = track[self.y][self.x]
        if new_track_piece == Track.INTERSECTION:
            self.dir = turn_new_dirs[self.next_turn][self.dir]
            self.next_turn = next(self.turn_cycle)
        else:
            self.dir = curve_new_dirs.get(new_track_piece, {}).get(self.dir, self.dir)

    def sort_val(self):
        # NOTE this assumes track size is never > 1000
        return self.y * 1000 + self.x

    def crashed_into(self, carts):
        for cart in carts:
            if cart and cart != self and cart.x == self.x and cart.y == self.y:
                return cart
        return None

    def __str__(self):
        return f"Cart {id(self)} at ({self.x}, {self.y}), dir {self.dir}"


track_piece_mapping = {
    " ": Track.BLANK,
    "|": Track.VERTICAL,
    "-": Track.HORIZONTAL,
    "/": Track.CURVE_NE,
    "\\": Track.CURVE_NW,
    "+": Track.INTERSECTION,
}

car_piece_mapping = {
    "^": (Track.VERTICAL, Direction.UP),
    "v": (Track.VERTICAL, Direction.DOWN),
    ">": (Track.HORIZONTAL, Direction.RIGHT),
    "<": (Track.HORIZONTAL, Direction.LEFT),
}


def part1(env):
    track, carts = _read_data(env)
    if env.test:
        print("expected crash loc = (7, 3)")
    print(_first_crash_loc(track, carts))


def part2(env):
    track, carts = _read_data(env)
    if env.test:
        print("expected final loc = (6, 4)")
    print(_last_cart_standing_loc(track, carts))


def _read_data(env):
    lines = data_file_lines(env)
    track = []
    carts = []
    for row_num, line in enumerate(lines):
        row = [Track.BLANK] * len(line)
        for col_num, piece in enumerate(line):
            tp = track_piece_mapping.get(piece)
            if tp is None:
                tp, car_dir = car_piece_mapping[piece]
                carts.append(Cart(col_num, row_num, car_dir))
            row[col_num] = tp
        track.append(row)
    return (track, carts)


def _first_crash_loc(track, carts):
    while True:
        carts.sort(key=Cart.sort_val)
        for cart in carts:
            cart.move(track, carts)
            if cart.crashed_into(carts):
                return (cart.x, cart.y)


def _last_cart_standing_loc(track, carts):
    while True:
        carts.sort(key=Cart.sort_val)
        crashed_carts = []
        for i in range(len(carts)):
            cart = carts[i]
            if cart is None:
                continue
            cart.move(track, carts)
            crashed_into = cart.crashed_into(carts)
            if crashed_into:
                carts[i] = None
                carts[carts.index(crashed_into)] = None
        remaining = [c for c in carts if c]
        if len(remaining) == 1:
            survivor = remaining[0]
            return (survivor.x, survivor.y)
        carts = remaining
