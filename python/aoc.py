#!/usr/bin/env python
#
# usage: aoc year day 1|2 [t]
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
parser.add_argument("-y", "--year", help="year")
parser.add_argument("-d", "--day", help="day")
parser.add_argument("-t", "--test", help="test mode", action="store_true")
parser.add_argument("part_number", help="puzzle part number (1 or 2)")
args = parser.parse_args()
if args.year:
    year = int(args.year)
if args.day:
    day = int(args.day)

module = importlib.import_module(f"y{year}.day{'%02d' % day}")
getattr(module, f"part{int(args.part_number)}")(args.test)
