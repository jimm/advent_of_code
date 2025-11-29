# Advent of Code

http://adventofcode.com/

I like solving these in multiple languages. For the first few years it was
Elixir. Now it's Ruby, Python, Elixir, Common Lisp, or whatever other
language I choose.

## The `aoc` Script

The bash script `aoc` in this directory will run any solution or test for
any year, and can initialize a day by copying a template and downloading the
initial data file if there is one (See "_Initializing a Day_" below). Run
`aoc --help` to see all of the options.

The `makeup` command used by the `-m` flag is a script of mine that finds
the nearest Makefile, Rakefile, or similar build file and runs the
corresponding make tool. The `-m` flag runs `makeup` before running the
solution.

Warning: over the years I've improved the support code for some of the
languages in ways thar are not always backwards compatible. So some of the
older solutions might not run out of the box.

## The Data

Per the preference of the author of Advent of Code
([Reddit comment](https://www.reddit.com/r/adventofcode/comments/7lesj5/is_it_kosher_to_share_puzzle_inputs_and_answers/drlt9am/)),
my data inputs are not available in this repo. Instead I store them
elsewhere and create a link to that data directory into this directory.

If you want to use much of this code and the `aoc` script, you'll need to
know my naming convention and the data format for the test input files.
The data for each day are stored in `data/yYYYY/dayDD_P.txt` where `YYYY` is
the year, `DD` is the two-digit day number, and `P` is the part number 1 or
2. Test files are stored in `data/yYYYY/dayDD_P_test.txt`.

The data file lookup code is written such that if a part 2 data or test file
does not exist, it looks for a part 1 version because very often the data is
shared between the two parts; only the expected answers differ. The next
section explains how that works.

### Test File Format

Each test file contains one or more sets of test data and expected answers
(collectively called "chunks") for either one part or for both parts 1 and 2
if both parts share the same test data but have different expected answers.
The expected answers come first, followed by the lines of input test data.

```
# test-1-expected-answer-part-1[,test-1-expected-answer-part-2]
test 1 test data
test 1 more test data

# test-2-expected-answer-part-1[,test-2-expected-answer-part-2]
test 2 test data
test 2 more test data
```

To split up the chunks we look at the first character of the first line to
see what character is used to signify the expected answer line. I most often
use "#", but since some data can start with "#" I might use ";" or something
else. The test data file is split into chunks based on that character.
Trailing blank lines, if any, are removed from each chunk.

In the case that both parts use the same test data, you'd find the expected
answers for parts 1 and two on the first lines of each chunk. That example
would probably be in `day_XX_1_test.txt` file because the test code will, if
running part 2 but there is no part 2 test data file, look for the part 1
test data file.

## Initializing a Day

The `aoc -i` command initializes a day by copying a template file into the
right place and optionally attempting to download the user's data file for
that day if it does not exist. The day to initialize is by default the
current day, but that can be changed by using other command line arguments.
See `aoc --help` for the list.

### Downloading the User's Data File

If the environment variable `AOC_COOKIE` is set or the `-c cookie` command
line argument to `aoc` is used, then the first data file will also be
fetched if it exists. You can get the cookie from inspecting any request to
adventofcode.com in your browser while you are logged in. It will look like
`session=some-hex-string`. It should be good all season, and perhaps over
multiple years.

    export AOC_COOKIE="session=..."
