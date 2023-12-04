# Advent of Code

http://adventofcode.com/

I like solving these in multiple languages. For the first few years it was
Elixir. Now it's Python, Elixir, Common Lisp, or whatever other language I
choose.

## The `aoc` Script

The bash script `aoc` in this directory will run any solution or test for
any year, and can initialize a day by copying a template and downloading the
initial data file if there is one. Run `aoc --help` to see all of the
options.

The `makeup` command used by the `-m` flag is a script of mine that finds
the nearest Makefile, Rakefile, or similar build file and runs the
corresponding make tool. The `-m` flag runs `makeup` before running the
solution.

## Initializing a Day

If the environment variable `AOC_COOKIE` is set, then the first data file
will also be fetched if one is given. You can get the cookie from inspecting
any request to adventofcode.com in your browser while you are logged in. It
will look like `session=some-hex-string`.

    export AOC_COOKIE="session=..."

## Running the Solutions

Any language's solution can be run using the `aoc` script.

The Ruby solutions are set up so that you can run the solution file and pass
it a subset of the same args (`--year` or `-y`, `--day` or `-d`, and
`--testing` or `-t`).
