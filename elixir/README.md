# Advent of Code Elixir Solutions

http://adventofcode.com/

## Running the challenges

The `../aoc` script runs `Common.AoC.run()` in [aoc.ex](lib/common/aoc.ex)
after setting up a couple of command line arguments. That function parses
the command line and calls the appropriate `Y<YEAR>.Day<DAY>.part<N>`
function, passing in the runtime context and a list of the lines read from
the corresponding data file (see the `Common.Data` module in
[data.ex](lib/common/data.ex)).

If the `-t` test flag is given, the corresponding test file is read and each
of the tests within it are run, passing the test lines into the proper
`part<N>` function. Failing tests' expected and actual values are reported.
See `Common.AoC.run_test_chunks` and the [main README](../README.md) for
more information about tests.

Run `../aoc --help` for more information about that script.

**NOTE**: years before 2025 were written differently. I've spent a few
minutes trying to "upgrade" those solutions, and they all compile, but I
haven't really tested them and it might take a little work to get them to
run again.

## Writing a solution

To initialize a new solution file use the `../aoc` script's `-i` flag. It
copies the template into the correct file and substitutes the puzzle name
and proper module name. If you have defined the AoC cookie, it will also
fetch the first data file and save it to `../data/yYYYY`.

See the [main README](../README.md) for more information about data files.
