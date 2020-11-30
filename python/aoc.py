#!/usr/bin/env python
#
# usage: aoc [--year year] [--day day] [--test] 1|2
#
# 't' for test

import argparse
import datetime
import importlib
import os.path
import sys

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
getattr(module, f"part{int(args.part_number)}")(args.test)
