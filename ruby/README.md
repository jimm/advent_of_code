# Ruby Advent of Code Framework

A structured framework for solving Advent of Code puzzles using the `Day`
class (ruby/day.rb).

## Quick Start

Use the `../aoc` script's `-i` flag to copy the template and initialize the
day's puzzle name and data. Or you can manually copy `template.rb` to your
year directory (e.g., `ruby/y2023/day01.rb`). Then implement `do_part1` and
`do_part2`:

```ruby
class Day01 < Day
  def do_part1(lines)
    # lines is an array of strings from the data file
    # Return the answer
  end

  def do_part2(lines)
    # Return the answer
  end
end
```

Run with:
```bash
../aoc 1 # year and day default to today, can be overridden
# or to run manually manually
ruby y2023/day01.rb 1      # Run part 1 with actual data
ruby y2023/day01.rb -t 1   # Run part 1 tests
ruby y2023/day01.rb -D 2   # Run part 2 with debug output
```

## Data Files

### File Naming and Location

Data files live in `data/y<year>/`:
- `day<NN>_<part>.txt` - Puzzle input (e.g., `day01_1.txt`, `day01_2.txt`)
- `day<NN>_<part>_test.txt` - Test input (e.g., `day01_1_test.txt`)

### Fallback Logic

When loading data, the framework searches in order:
1. `day<NN>_<part>[_test].txt` (exact match)
2. `day<NN>_1[_test].txt` (part 1 fallback for part 2)
3. `day<NN>[_test].txt` (no part number)

This lets part 2 reuse part 1 data when they're the same.

### Data Format

The `lines` parameter passed to your methods is:
- Array of strings, one per line
- Line endings stripped via `chomp`
- Empty lines removed by default
- `nil` if no data file exists

## Testing

### Test File Format

Test files use a chunk-based format for multiple test cases:

```
# <expected>
<data lines>

# <expected>
<data lines>
```

The first character (here `#`) is the delimiter. Any line starting with that character begins a new test chunk.

**Expected Values**: Use comma to specify different answers for parts 1 and 2:
- `# 42` - Both parts expect 42
- `# 42,100` - Part 1 expects 42, part 2 expects 100

Example (`day01_1_test.txt`):
```
# 142
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
```

### How Tests Work

When you run with `-t`, the framework (ruby/day.rb:154):
1. Finds test file using the same fallback logic as data files
2. Parses chunks with `test_chunks()` (ruby/day.rb:225)
3. For each chunk, calls your `do_part<N>` with the test data lines
4. Compares result to expected value
5. Prints `.` for pass, `F` for failure

Output:
```
....F..
run 5: expected 42, got 43
```

### Custom Tests

Override test methods if needed:

```ruby
def part1_tests
  run_one_test(42) { do_part1(["test", "data"]) }
end

def do_tests(part_number = @part_number)
  # Completely custom test logic
end
```

## Command-Line Interface

It's easiest to use `../aoc` to launch tests most of the time, but here's
how to run things "manually":

```bash
ruby y2023/day01.rb [options] <part_number>
```

**Options**:
- `-t`, `--testing` - Run tests instead of solution
- `-D`, `--debug` - Enable debug output (`$DEBUG = true`)
- `-y YEAR`, `--year YEAR` - Override year
- `-d DAY`, `--day DAY` - Override day

The `aoc(__FILE__)` call (ruby/aoc.rb) parses year and day from the file path, processes options, instantiates your Day class, and calls `run()` (ruby/day.rb:43).

## Debugging Helpers

Available when `$DEBUG` is true (ruby/day.rb:1-26):

```ruby
value.pdebug("msg")    # Print value.to_s, return self (chainable)
value.idebug("msg")    # Print value.inspect, return self
debug("message")       # Print message
clear_screen           # Clear terminal
```

Example:
```ruby
lines.map { |x| x.to_i }.pdebug("converted").sum
```

## Instance Variables

Available in your Day subclass:
- `@year` - Year (Integer)
- `@day` - Day of month (Integer)
- `@part_number` - Current part (1 or 2)
- `@testing` - Boolean, true if running tests

## Key Methods

**You implement**:
- `do_part1(lines)` - Calculate and return part 1 answer (ruby/day.rb:60)
- `do_part2(lines)` - Calculate and return part 2 answer (ruby/day.rb:80)

**Framework provides**:
- `part1(lines = nil)` - Loads data, calls `do_part1`, prints result (ruby/day.rb:52)
- `part2(lines = nil)` - Loads data, calls `do_part2`, prints result (ruby/day.rb:72)
- `data_lines(part, skip_empty = true)` - Get data as array of lines (ruby/day.rb:204)
- `test_chunks(part)` - Parse test file into chunks (ruby/day.rb:225)
- `run_chunk_tests(part)` - Run all test chunks (ruby/day.rb:113)

## Tips

1. Return answers from `do_part1`/`do_part2`, don't print them
2. Use `-t` frequently to run tests during development
3. Add `.pdebug` calls with `-D` flag for debugging
4. One test file with comma-separated values handles both parts
5. Create only `day<NN>_1.txt` if both parts share input
6. Start from `template.rb` for correct structure
