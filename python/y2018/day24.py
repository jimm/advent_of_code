# Immune System Simulator 20XX

import re

from utils import *


class Army:
    def __init__(self):
        self.groups = []


class Group:
    def __init__(
        self, count, hp, attack_type, attack_hp, initiative, immunities, weaknesses
    ):
        self.count = count
        self.hp = hp
        self.attack_type = attack_type
        self.attack_hp = attack_hp
        self.initiative = initiative
        self.immunities = immunities
        self.weaknesses = weaknesses

    def effective_power(self):
        return self.count * self.attack_hp


def part1(env):
    armies = _read_armies(env)
    print(armies)  # DEBUG


def part2(env):
    lines = data_file_lines(env)


def _read_armies(env):
    armies = []
    for line in data_file_lines(env):
        if line[-1] == ":":
            army = Army()
            armies.append(army)
            continue

        m = re.match(
            r"(\d+) units each with (\d+) hit points \((.*?)\) with an attack that does (\d+) (.*?) at initiative (\d+)",
            line,
        )
        units = int(m[1])
        hp = int(m[2])
        info = m[3]
        attack = int(m[4])
        attack_type = m[5]
        initiative = int(m[6])
        immunities = []
        weaknesses = []
        for sub_info in info.split(";"):
            if sub_info.startswith("immune to "):
                immunities = sub_info[10:].split(", ")
            elif sub_info.startswith("weak to "):
                weaknesses = sub_info[8:].split(", ")
        army.groups.append(
            Group(units, hp, attack_type, attack, initiative, immunities, weaknesses)
        )
    return armies
