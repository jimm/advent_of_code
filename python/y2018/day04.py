# Repose Record
#
# Don't need to track date or timestamp, just need nap minute ranges. Could
# get rid of all timestamps/dates, but no need to bother. Runs plenty fast.

from utils import *


class Nap:
    def __init__(self, guard_id, date, nap_start_min, nap_end_min):
        self.guard_id = guard_id
        self.date = date
        self.nap = range(nap_start_min, nap_end_min)

    def __len__(self):
        return len(self.nap)

    def __repr__(self):
        return f"Nap({self.guard_id}, {self.date}, {self.nap})"


def part1(testing=False):
    min_day_start, max_day_start, guard_naps = _parse_data(testing, 1)
    num_minutes_asleep = {}
    for guard_id, naps in guard_naps.items():
        num_minutes_asleep[guard_id] = 0
        for nap in naps:
            num_minutes_asleep[guard_id] += len(nap)
    sleepiest_guard_id = _sleepiest_guard(num_minutes_asleep)
    # answer is sleepiest guard id times sleepiest minute number
    sleepiest_minute, _ = _sleepiest_minute(guard_naps[sleepiest_guard_id])
    print(sleepiest_guard_id * sleepiest_minute)


def part2(testing=False):
    min_day_start, max_day_start, guard_naps = _parse_data(testing, 1)
    sleepiest_minutes = _sleepiest_minutes(guard_naps)
    reversed = {
        count: (guard_id, max_minute)
        for guard_id, (max_minute, count) in sleepiest_minutes.items()
    }
    max_count = max(reversed)
    sleepiest_guard_id, sleepiest_minute = reversed[max_count]
    # answer is sleepiest guard id times most frequently asleep minute number
    print(sleepiest_guard_id * sleepiest_minute)


# Return min day start as timestamp, max day start as timestamp, and dict
# with guard_id as key and list of Naps as value.
def _parse_data(testing, part_num):
    lines = sorted(data_file_lines("04", testing, part_num))
    tstamp = None
    min_day_start = None
    naps = []
    guard_id = None
    sleep_start = None
    for line in lines:
        tstr = line[1:17]
        tstamp = _timestamp(tstr)
        min_day_start = min_day_start or int(tstamp / 10000) * 10000
        action = line[19:]
        if action.startswith("Guard #"):
            guard_id = int(action.split()[1][1:])
        elif action == "falls asleep":
            sleep_start = tstr
        else:
            naps.append(
                Nap(guard_id, tstr[0:10], int(sleep_start[14:16]), int(tstr[14:16]))
            )
    max_day_start = min_day_start or int(tstamp / 10000) * 10000 + 60

    guard_naps = {}  # key = guard id, val = list of ranges
    for nap in naps:
        guard_naps[nap.guard_id] = guard_naps.get(nap.guard_id, [])
        guard_naps[nap.guard_id].append(nap)

    return (min_day_start, max_day_start, guard_naps)


def _timestamp(s):
    ts = s[5:7] + s[8:10] + s[11:13] + s[14:16]
    return int(ts)


def _sleepiest_guard(num_minutes_asleep):
    reversed = {num_mins: guard_id for guard_id, num_mins in num_minutes_asleep.items()}
    return reversed[max(reversed)]


# Given one guard's naps, return a tuple containing (the minute in which the
# guard tends to sleep most often, the number of times that is the case).
def _sleepiest_minute(guard_naps):
    sleep_by_minute = [0] * 60
    for nap in guard_naps:
        for i in nap.nap:
            sleep_by_minute[i] += 1
    reversed = {sleeps: minute for minute, sleeps in enumerate(sleep_by_minute)}
    max_num_sleeps = max(reversed)
    return (reversed[max_num_sleeps], max_num_sleeps)


# Returns dict with guard_id as key and (sleepiest minute, max mins asleep)
# tuple as value.
def _sleepiest_minutes(guard_naps):
    smins = {}
    for guard_id, naps in guard_naps.items():
        sleepiest_minute = None
        max_minutes_asleep = 0
        minute, count = _sleepiest_minute(naps)
        if count > max_minutes_asleep:
            max_minutes_asleep = count
            sleepiest_minute = minute
        smins[guard_id] = (sleepiest_minute, max_minutes_asleep)
    return smins
