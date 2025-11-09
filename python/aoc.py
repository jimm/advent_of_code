#!/usr/bin/env python
#
# usage: aoc.py [-y|--year year] [-d|--day day] [-t|--test] 1|2

import argparse
import datetime
import importlib
import os.path
import sys

import .testing

now = datetime.datetime.today()
year = now.year
day = now.day

parser = argparse.ArgumentParser()
parser.add_argument("-y", "--year", type=int, help="year")
parser.add_argument("-d", "--day", type=int, help="day")
parser.add_argument("-t", "--test", action="store_true", help="test mode")
parser.add_argument("part_number", type=int, help="puzzle part number (1 or 2)")
args = parser.parse_args()
if args.year:
    year = int(args.year)
if args.day:
    day = int(args.day)

module = importlib.import_module(f"y{year}.day{'%02d' % day}")

# NOTE: this will only work for years 2025+ because in the last few years
# I've drastically changed how I run tests. See ../ruby. Previous years'
# solutions won't work with run_chunk_tests, they have the testing code
# built in. This code won't handle that.
func = getattr(module, f"part{int(args.part_number)}", None)
if args.test:
    testing.run_chunk_tests(args, lambda expected, lines: expected == func(ctx, lines))
else:
    print(func(args))
