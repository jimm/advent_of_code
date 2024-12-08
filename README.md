# Advent of Code

http://adventofcode.com/

I like solving these in multiple languages. For the first few years it was
Elixir. Now it's Ruby, Python, Elixir, Common Lisp, or whatever other
language I choose.

## The `aoc` Script

The bash script `aoc` in this directory will run any solution or test for
any year, and can initialize a day by copying a template and downloading the
initial data file if there is one (See _Initializing a Day_ below). Run `aoc
--help` to see all of the options.

The `makeup` command used by the `-m` flag is a script of mine that finds
the nearest Makefile, Rakefile, or similar build file and runs the
corresponding make tool. The `-m` flag runs `makeup` before running the
solution.

## The Data

Per the preference of the author of Advent of Code
([Reddit comment](https://www.reddit.com/r/adventofcode/comments/7lesj5/is_it_kosher_to_share_puzzle_inputs_and_answers/drlt9am/)),
I should not be making all of my data inputs available in this repo. So I've
removed them and will instead store them elsewhere and create a link to that
data directory into this directory, which is now ignored by git (see
`.gitignore`).

If you want to use much of this code, you'll need to know my naming
convention and the data format for the test input files.

The files for each day are stored in `data/yYYYY/dayDD_P.txt` where `YYYY` is
the year, `DD` is the two-digit day number, and `P` is the part number 1 or
2. Test files are stored in `data/yYYYY/dayDD_P_test.txt`.

The data file lookup code is written such that if a part 2 data or test file
does not exist, it looks for a part 1 version because very often the data is
shared between the two parts; only the expected answers differ.

### Test File Format

Each test file contains one or more sets of test data and expected answers
(collectively called "chunks") for both part 1 and part 2. The expected
answers come first, followed by the lines of input test data.

Since some data can start with "#", to split up the chunks we look at the
first character of the first line to see what character is used to signify
the expected answer line. It's most "#" when the data will not start with
that character, but might be ";" or something else. Next the test data file
is split into chunks based on that character. Finally, trailing blank lines
are removed from each chunk.

```
# expected-answer-part-1[,expected-answer-part-2]
test 1 test data
test 1 more test data

# t2-expected-answer-part-1[,t2-expected-answer-part-2]
test 2 test data
test 2 more test data
```

## Initializing a Day

If the environment variable `AOC_COOKIE` is set or the `-c cookie` command
line argument to `aoc` is used, then the first data file will also be
fetched if it exists. You can get the cookie from inspecting any request to
adventofcode.com in your browser while you are logged in. It will look like
`session=some-hex-string`. It should be good all season.

    export AOC_COOKIE="session=..."
