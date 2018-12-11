# Chronal Charge
#
# Note, answer must be entered with no spaces. For example, coord (23, 45)
# must be entered as "23,45".

# part 2 answer 220,103,22 is wrong

from utils import *

puzzle_input = 9445
power_level_tests = [
    # x, y, serial_num, expected
    (3, 5, 8, 4),
    (122, 79, 57, -5),
    (217, 196, 39, 0),
    (101, 153, 71, 4),
]
puzzle_tests = [
    # serial number, ((exp x, exp y), exp level)
    (18, ((33, 45), 29)),
    (42, ((21, 61), 30)),
]
# Holds {coord: power}. Size of subgrid is implicitly (current size - 1).
subgrid_power_cache = {}


def part1(testing=False):
    if testing:
        for test in power_level_tests:
            _run_power_level_test(test)
        for test in puzzle_tests:
            _run_puzzle_test(test)
        print("ok")
    else:
        grid = _init_grid(puzzle_input, 300)
        print(_max_power_subgrid(grid, 3))


def part2(testing=False):
    grid = _init_grid(puzzle_input, 300)
    max_coord, max_size, max_power = None, None, 0
    for size in range(3, 301):
        print(size)
        coord, power = _max_power_subgrid(grid, size)
        if power > max_power:
            max_coord = coord
            max_size = size
            max_power = power
    print((max_coord, max_size, max_power))


def _init_grid(serial_number, grid_size):
    range_max = grid_size + 1
    grid = [[0 for x in range(range_max)] for y in range(range_max)]
    for x in range(1, range_max):
        # Reduce number of calculations by doing a few up front first
        rack_id = x + 10
        rack_id_squared = rack_id * rack_id
        rack_id_times_serial_number = rack_id * serial_number
        pl = rack_id_squared + rack_id_times_serial_number
        for y in range(1, range_max):
            # power_level = ((rack_id * y) + serial_number) * rack_id
            power_level = pl
            hundreds_digit = int(power_level / 100) % 10
            grid[y][x] = hundreds_digit - 5
            pl += rack_id_squared
    return grid


def _max_power_subgrid(grid, size):
    max_power = 0
    subgrid_coord = None
    range_max = len(grid[0]) - (size - 1)
    for x in range(1, range_max):
        for y in range(1, range_max):
            subgrid_power = _subgrid_power(grid, x, y, size)
            if subgrid_power > max_power:
                max_power = subgrid_power
                subgrid_coord = (x, y)
    return (subgrid_coord, max_power)


def _subgrid_power(grid, top_left_x, top_left_y, size):
    global subgrid_power_cache
    coord = (top_left_x, top_left_y)
    power = 0
    if coord in subgrid_power_cache:
        power = subgrid_power_cache[coord]
        x = top_left_x + size - 1
        for y in range(top_left_y, top_left_y + size):
            power += grid[y][x]
    else:
        for y in range(top_left_y, top_left_y + size):
            power += sum(grid[y][top_left_x : (top_left_x + size)])
    subgrid_power_cache[coord] = power
    return power


def _run_power_level_test(test):
    global subgrid_power_cache
    subgrid_power_cache = {}
    x, y, serial_number, expected = test
    grid = _init_grid(serial_number, max(x, y) + 3)
    power = grid[y][x]
    if power != expected:
        raise Exception(
            f"({x}, {y}), sn {serial_number} expected {expected} but saw {power}"
        )


def _run_puzzle_test(test):
    global subgrid_power_cache
    subgrid_power_cache = {}
    serial_number, expected_answer = test
    grid = _init_grid(serial_number, 300)
    answer = _max_power_subgrid(grid, 3)
    if answer != expected_answer:
        raise Exception(f"expected: {expected_answer}, seen: {answer}")
