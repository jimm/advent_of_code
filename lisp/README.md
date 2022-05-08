First install [Quicklisp](https://www.quicklisp.org/beta/).

The simplest way to run a day's answers is to use the `../aoc` script.

The dan's files are written to be run from within the year directory --- the
paths to the data files are relative to those year directories. To run a
day's file manually,

```
$ cd y2017
$ sbcl # or however you start your REPL
* (load "../utils.lisp")
* (load "day01.lisp")
* (part1)
```

NOTE: I originally entered year 2017 was under a different Advent of Code
account than I use now (my Github account). The first few data files have
been updated to the new account's data files and the solutions for those
days have been entered.
