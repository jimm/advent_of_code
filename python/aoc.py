#!/usr/bin/env python
#
# usage: aoc year day 1|2 [t]
#
# 't' for test

import importlib
import os.path
import sys

if len(sys.argv) < 4:
    print("usage: aoc.py year day 1|2 [t]")
    exit(1)
year = int(sys.argv[1])
day = int(sys.argv[2])
part_num = int(sys.argv[3])
testing = len(sys.argv) >= 5

module = importlib.import_module(f"y{year}.day{'%02d' % day}")
getattr(module, f"part{part_num}")(testing)
