# Python Advent of Code Framework

A structured framework for solving Advent of Code puzzles using utility
functions from `testing.py` and `utils.py`.

**Note**: This framework is for years 2025+. Earlier years use different
testing approaches incompatible with `run_chunk_tests`.

## Quick Start

Use the `../aoc` script's `-i` flag to copy the template and initialize the
day's puzzle name and data. Or you can manually copy `template.py` to your
year directory (e.g., `python/y2025/day01.py`). Then implement `part1` and
`part2`:

```python
# Puzzle Name

from testing import data_file_lines

def part1(ctx):
    lines = data_file_lines(ctx)
    # Calculate and print answer
    print(answer)

def part2(ctx):
    lines = data_file_lines(ctx)
    # Calculate and print answer
    print(answer)
```

Run with:
```bash
../aoc 1 # year and day default to today, can be overridden
# or to run manually manually
python -m aoc -d 1 1          # Run day 1 part 1 with actual data
python -m aoc -d 1 -t 1       # Run day 1 part 1 tests
python -m aoc -y 2023 -d 1 2  # Run specific year/day
```

## Data Files

### File Naming and Location

Data files live in `data/y<year>/`:
- `day<NN>_<part>.txt` - Puzzle input (e.g., `day01_1.txt`, `day01_2.txt`)
- `day<NN>_<part>_test.txt` - Test input (e.g., `day01_1_test.txt`)

### Fallback Logic

When loading data, the framework searches in order (testing.py:36-40):
1. `day<NN>_<part>[_test].txt` (exact match)
2. `day<NN>_1[_test].txt` (part 1 fallback for part 2)
3. `day<NN>[_test].txt` (no part number)

This lets part 2 reuse part 1 data when they're the same.

## Getting Data

### `data_file_lines(ctx, preserve_blank_lines=False)`

Returns lines from the data file (testing.py:85):
- **Default** (`preserve_blank_lines=False`): List of non-blank lines as
  strings
- **preserve_blank_lines=True**: List of lists, grouped by blank line
  separators

Examples:
```python
# Get all non-blank lines
lines = data_file_lines(ctx)  # ["line1", "line2", "line3"]

# Get line groups separated by blank lines
groups = data_file_lines(ctx, preserve_blank_lines=True)
# [["line1", "line2"], ["line3", "line4"]]
```

### `read_data_file(ctx)`

Returns entire file contents as a single string (testing.py:22).

## Testing

### Test File Format

Test files use chunk-based format (same as Ruby):

```
# <expected>
<data lines>

# <expected>
<data lines>
```

The first character (here `#`) is the delimiter. Any line starting with that
character begins a new test chunk.

**Expected Values**: Use comma to specify different answers for parts 1 and 2:
- `# 42` - Both parts expect 42
- `# 42,100` - Part 1 expects 42, part 2 expects 100

### How Tests Work

When you run with `-t`, `run_chunk_tests` (testing.py:49):
1. Finds test file using fallback logic
2. Parses chunks with `test_chunks()` (testing.py:103)
3. For each chunk, calls your `part<N>` function
4. Compares result to expected value
5. Prints `.` for pass, `F` for failure

The test function you provide must return a tuple:
- `(bool, answer)` - Pass/fail and actual answer
- `(bool, answer, expected)` - Pass/fail, actual answer, and override expected

Output:
```
....F..
expected 42, got 43
```

## Command-Line Interface

It's easiest to use `../aoc` to launch tests most of the time, but here's
how to run things "manually":

```bash
python -m aoc [options] <part_number>
```

**Options**:
- `-y YEAR`, `--year YEAR` - Specify year (default: current year)
- `-d DAY`, `--day DAY` - Specify day (default: current day)
- `-t`, `--test` - Run tests instead of solution

The `aoc.py` script (aoc.py:1):
1. Parses command-line arguments
2. Imports module `y{year}.day{NN}`
3. Gets function `part{N}` from module
4. Calls function with context object or runs tests

## Context Object

The `ctx` parameter passed to your functions contains:
- `ctx.year` - Year (int)
- `ctx.day` - Day of month (int)
- `ctx.part_number` - Current part (1 or 2)
- `ctx.test` - Boolean, True if running tests

## Utility Functions

### From `testing.py`

- `data_file_lines(ctx, preserve_blank_lines=False)` - Get lines from data
  file (testing.py:85)
- `read_data_file(ctx)` - Get entire file as string (testing.py:22)
- `test_chunks(ctx)` - Parse test file into chunks (testing.py:103)
- `run_chunk_tests(ctx, func)` - Run all test chunks (testing.py:49)

### From `utils.py`

- `minmax(xs)` - Returns `(min, max)` tuple from iterable (utils.py:4)
- `flatten(xs)` - Flattens nested lists (utils.py:16)
- `pause()` - Interactive debugging pause (utils.py:29)

## Tips

1. Print answers from `part1`/`part2`, don't return them
2. Use `-t` frequently to run tests during development
3. Import `from testing import *` or `from utils import *` for convenience
4. One test file with comma-separated values handles both parts
5. Create only `day<NN>_1.txt` if both parts share input
6. Start from `template.py` for correct structure
7. For grouped data (separated by blank lines), use `preserve_blank_lines=True`
